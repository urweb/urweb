(* Copyright (c) 2008, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

structure JsComp :> JSCOMP = struct

open Mono

structure EM = ErrorMsg
structure E = MonoEnv
structure U = MonoUtil

val funcs = [(("Basis", "alert"), "alert"),
             (("Basis", "htmlifyFloat"), "ts"),
             (("Basis", "htmlifyInt"), "ts"),
             (("Basis", "htmlifyString"), "escape"),
             (("Basis", "new_client_source"), "sc"),
             (("Basis", "set_client_source"), "sv")]

structure FM = BinaryMapFn(struct
                           type ord_key = string * string
                           fun compare ((m1, x1), (m2, x2)) =
                               Order.join (String.compare (m1, m2),
                                           fn () => String.compare (x1, x2))
                           end)

val funcs = foldl (fn ((k, v), m) => FM.insert (m, k, v)) FM.empty funcs

fun ffi k = FM.find (funcs, k)

type state = {
     decls : decl list,
     script : string
}

fun varDepth (e, _) =
    case e of
        EPrim _ => 0
      | ERel _ => 0
      | ENamed _ => 0
      | ECon (_, _, NONE) => 0
      | ECon (_, _, SOME e) => varDepth e
      | ENone _ => 0
      | ESome (_, e) => varDepth e
      | EFfi _ => 0
      | EFfiApp (_, _, es) => foldl Int.max 0 (map varDepth es)
      | EApp (e1, e2) => Int.max (varDepth e1, varDepth e2)
      | EAbs _ => 0
      | EUnop (_, e) => varDepth e
      | EBinop (_, e1, e2) => Int.max (varDepth e1, varDepth e2)
      | ERecord xes => foldl Int.max 0 (map (fn (_, e, _) => varDepth e) xes)
      | EField (e, _) => varDepth e
      | ECase (e, pes, _) =>
        foldl Int.max (varDepth e)
        (map (fn (p, e) => E.patBindsN p + varDepth e) pes)
      | EStrcat (e1, e2) => Int.max (varDepth e1, varDepth e2)
      | EError (e, _) => varDepth e
      | EWrite e => varDepth e
      | ESeq (e1, e2) => Int.max (varDepth e1, varDepth e2)
      | ELet (_, _, e1, e2) => Int.max (varDepth e1, 1 + varDepth e2)
      | EClosure _ => 0
      | EQuery _ => 0
      | EDml _ => 0
      | ENextval _ => 0
      | EUnurlify _ => 0
      | EJavaScript _ => 0
      | ESignalReturn e => varDepth e
      | ESignalBind (e1, e2) => Int.max (varDepth e1, varDepth e2)
      | ESignalSource e => varDepth e

fun strcat loc es =
    case es of
        [] => (EPrim (Prim.String ""), loc)
      | [x] => x
      | x :: es' => (EStrcat (x, strcat loc es'), loc)

fun jsExp mode skip outer =
    let
        val len = length outer

        fun jsE inner (e as (_, loc), st) =
            let
                fun str s = (EPrim (Prim.String s), loc)

                fun var n = Int.toString (len + inner - n - 1)

                fun patCon pc =
                    case pc of
                        PConVar n => str (Int.toString n)
                      | PConFfi {con, ...} => str ("\"_" ^ con ^ "\"")

                fun isNullable (t, _) =
                    case t of
                        TOption _ => true
                      | TRecord [] => true
                      | _ => false

                fun unsupported s =
                  (EM.errorAt loc (s ^ " in code to be compiled to JavaScript");
                   (str "ERROR", st))

                val strcat = strcat loc

                fun quoteExp (t : typ) e =
                    case #1 t of
                        TSource => strcat [str "s",
                                           (EFfiApp ("Basis", "htmlifyInt", [e]), loc)]
                      | TRecord [] => str "null"
                      | TFfi ("Basis", "string") => e
                      | _ => (EM.errorAt loc "Don't know how to embed type in JavaScript";
                              Print.prefaces "Can't embed" [("t", MonoPrint.p_typ MonoEnv.empty t)];
                              str "ERROR")

                fun jsPrim p =
                    case p of
                        Prim.String s =>
                        str ("\""
                             ^ String.translate (fn #"'" =>
                                                    if mode = Attribute then
                                                        "\\047"
                                                    else
                                                        "'"
                                                  | #"\"" => "\\\""
                                                  | #"<" =>
                                                    if mode = Script then
                                                        "<"
                                                    else
                                                        "\\074"
                                                  | #"\\" => "\\\\"
                                                  | ch => String.str ch) s
                             ^ "\"")
                      | _ => str (Prim.toString p)

                fun jsPat depth inner (p, _) succ fail =
                    case p of
                        PWild => succ
                      | PVar _ => strcat [str ("(_" ^ Int.toString (len + inner) ^ "=d" ^ Int.toString depth ^ ","),
                                          succ,
                                          str ")"]
                      | PPrim p => strcat [str ("(d" ^ Int.toString depth ^ "=="),
                                           jsPrim p,
                                           str "?",
                                           succ,
                                           str ":",
                                           fail,
                                           str ")"]
                      | PCon _ => raise Fail "jsPat: PCon"
                      | PRecord xps =>
                        let
                            val (_, succ) = foldl
                                            (fn ((x, p, _), (inner, succ)) =>
                                                (inner + E.patBindsN p,
                                                 strcat [str ("(d" ^ Int.toString (depth+1) ^ "=d"
                                                              ^ Int.toString depth ^ "._" ^ x ^ ","),
                                                         jsPat (depth+1) inner p succ fail,
                                                         str ")"]))
                                            (inner, succ) xps
                        in
                            succ
                        end
                      | PNone _ => strcat [str ("(d" ^ Int.toString depth ^ "?"),
                                           fail,
                                           str ":",
                                           succ,
                                           str ")"]
                      | PSome (_, p) => strcat [str ("(d" ^ Int.toString depth ^ "?"),
                                                jsPat depth inner p succ fail,
                                                str ":",
                                                fail,
                                                str ")"]
            in
                case #1 e of
                    EPrim p => (jsPrim p, st)
                  | ERel n =>
                    if n < inner then
                        (str ("_" ^ var n), st)
                    else
                        let
                            val n = n - inner
                        in
                            (quoteExp (List.nth (outer, n)) (ERel (n - skip), loc), st)
                        end
                  | ENamed _ => raise Fail "Named"
                  | ECon (_, pc, NONE) => (patCon pc, st)
                  | ECon (_, pc, SOME e) =>
                    let
                        val (s, st) = jsE inner (e, st)
                    in
                        (strcat [str "{n:",
                                 patCon pc,
                                 str ",v:",
                                 s,
                                 str "}"], st)
                    end
                  | ENone _ => (str "null", st)
                  | ESome (t, e) =>
                    let
                        val (e, st) = jsE inner (e, st)
                    in
                        (if isNullable t then
                             strcat [str "{v:", e, str "}"]
                         else
                             e, st)
                    end

                  | EFfi k =>
                    let
                        val name = case ffi k of
                                       NONE => (EM.errorAt loc ("Unsupported FFI identifier " ^ #2 k ^ " in JavaScript");
                                                "ERROR")
                                     | SOME s => s
                    in
                        (str name, st)
                    end
                  | EFfiApp (m, x, args) =>
                    let
                        val args =
                            case (m, x, args) of
                                ("Basis", "new_client_source", [(EJavaScript (_, e, _), _)]) => [e]
                              | ("Basis", "set_client_source", [e1, (EJavaScript (_, e2, _), _)]) => [e1, e2]
                              | _ => args

                        val name = case ffi (m, x) of
                                       NONE => (EM.errorAt loc ("Unsupported FFI function " ^ x ^ " in JavaScript");
                                                "ERROR")
                                     | SOME s => s
                    in
                        case args of
                            [] => (str (name ^ "()"), st)
                          | [e] =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str (name ^ "("),
                                         e,
                                         str ")"], st)
                            end
                          | e :: es =>
                            let
                                val (e, st) = jsE inner (e, st)
                                val (es, st) = ListUtil.foldlMapConcat
                                                   (fn (e, st) =>
                                                       let
                                                           val (e, st) = jsE inner (e, st)
                                                       in
                                                           ([str ",", e], st)
                                                       end)
                                                   st es
                            in
                                (strcat (str (name ^ "(")
                                         :: e
                                         :: es
                                         @ [str ")"]), st)
                            end
                    end

                  | EApp (e1, e2) =>
                    let
                        val (e1, st) = jsE inner (e1, st)
                        val (e2, st) = jsE inner (e2, st)
                    in
                        (strcat [e1, str "(", e2, str ")"], st)
                    end
                  | EAbs (_, _, _, e) =>
                    let
                        val locals = List.tabulate
                                     (varDepth e,
                                   fn i => str ("var _" ^ Int.toString (len + inner + i + 1) ^ ";"))
                        val (e, st) = jsE (inner + 1) (e, st)
                    in
                        (strcat (str ("function(_"
                                      ^ Int.toString (len + inner)
                                      ^ "){")
                                 :: locals
                                 @ [str "return ",
                                    e,
                                    str "}"]),
                         st)
                    end

                  | EUnop (s, e) =>
                    let
                        val (e, st) = jsE inner (e, st)
                    in
                        (strcat [str ("(" ^ s),
                                 e,
                                 str ")"],
                         st)
                    end
                  | EBinop (s, e1, e2) =>
                    let
                        val (e1, st) = jsE inner (e1, st)
                        val (e2, st) = jsE inner (e2, st)
                    in
                        (strcat [str "(",
                                 e1,
                                 str s,
                                 e2,
                                 str ")"],
                         st)
                    end

                  | ERecord [] => (str "null", st)
                  | ERecord [(x, e, _)] =>
                    let
                        val (e, st) = jsE inner (e, st)
                    in
                        (strcat [str "{_x:", e, str "}"], st)
                    end
                  | ERecord ((x, e, _) :: xes) =>
                    let
                        val (e, st) = jsE inner (e, st)

                        val (es, st) =
                            foldr (fn ((x, e, _), (es, st)) =>
                                      let
                                          val (e, st) = jsE inner (e, st)
                                      in
                                          (str (",_" ^ x ^ ":")
                                           :: e
                                           :: es,
                                           st)
                                      end)
                                  ([str "}"], st) xes
                    in
                        (strcat (str ("{_" ^ x ^ ":")
                                 :: e
                                 :: es),
                         st)
                    end
                  | EField (e, x) =>
                    let
                        val (e, st) = jsE inner (e, st)
                    in
                        (strcat [e,
                                 str ("._" ^ x)], st)
                    end

                  | ECase (e, pes, _) =>
                    let
                        val plen = length pes

                        val (cases, st) = ListUtil.foldliMap
                                              (fn (i, (p, e), st) =>
                                                  let
                                                      val (e, st) = jsE (inner + E.patBindsN p) (e, st)
                                                      val fail =
                                                          if i = plen - 1 then
                                                              str "pf()"
                                                          else
                                                              str ("c" ^ Int.toString (i+1) ^ "()")
                                                      val c = jsPat 0 inner p e fail
                                                  in
                                                      (strcat [str ("c" ^ Int.toString i ^ "=function(){return "),
                                                               c,
                                                               str "},"],
                                                       st)
                                                  end)
                                              st pes

                        val (e, st) = jsE inner (e, st)
                    in
                        (strcat (str "("
                                 :: List.revAppend (cases,
                                                    [str "d0=",
                                                     e,
                                                     str ",c0())"])), st)
                    end

                  | EStrcat (e1, e2) =>
                    let
                        val (e1, st) = jsE inner (e1, st)
                        val (e2, st) = jsE inner (e2, st)
                    in
                        (strcat [str "(", e1, str "+", e2, str ")"], st)
                    end

                  | EError (e, _) =>
                    let
                        val (e, st) = jsE inner (e, st)
                    in
                        (strcat [str "alert(\"ERROR: \"+", e, str ")"],
                         st)
                    end

                  | EWrite e =>
                    let
                        val (e, st) = jsE inner (e, st)
                    in
                        (strcat [str "document.write(",
                                 e,
                                 str ".v)"], st)
                    end

                  | ESeq (e1, e2) =>
                    let
                        val (e1, st) = jsE inner (e1, st)
                        val (e2, st) = jsE inner (e2, st)
                    in
                        (strcat [str "(", e1, str ",", e2, str ")"], st)
                    end
                  | ELet (_, _, e1, e2) =>
                    let
                        val (e1, st) = jsE inner (e1, st)
                        val (e2, st) = jsE (inner + 1) (e2, st)
                    in
                        (strcat [str ("(_" ^ Int.toString (len + inner) ^ "="),
                                 e1,
                                 str ",",
                                 e2,
                                 str ")"], st)
                    end

                  | EClosure _ => unsupported "EClosure"
                  | EQuery _ => unsupported "Query"
                  | EDml _ => unsupported "DML"
                  | ENextval _ => unsupported "Nextval"
                  | EUnurlify _ => unsupported "EUnurlify"
                  | EJavaScript _ => unsupported "Nested JavaScript"
                  | ESignalReturn e =>
                    let
                        val (e, st) = jsE inner (e, st)
                    in
                        (strcat [str "sr(",
                                 e,
                                 str ")"],
                         st)
                    end
                  | ESignalBind (e1, e2) =>
                    let
                        val (e1, st) = jsE inner (e1, st)
                        val (e2, st) = jsE inner (e2, st)
                    in
                        (strcat [str "sb(",
                                 e1,
                                 str ",",
                                 e2,
                                 str ")"],
                         st)
                    end
                  | ESignalSource e =>
                    let
                        val (e, st) = jsE inner (e, st)
                    in
                        (strcat [str "ss(",
                                 e,
                                 str ")"],
                         st)
                    end
            end
    in
        jsE
    end

val decl : state -> decl -> decl * state =
    U.Decl.foldMapB {typ = fn x => x,
                     exp = fn (env, e, st) =>
                              let
                                  fun doCode m skip env orig e =
                                      let
                                          val len = length env
                                          fun str s = (EPrim (Prim.String s), #2 e)

                                          val locals = List.tabulate
                                                           (varDepth e,
                                                         fn i => str ("var _" ^ Int.toString (len + i) ^ ";"))
                                          val (e, st) = jsExp m skip env 0 (e, st)
                                      in
                                          (EJavaScript (m, orig, SOME (strcat (#2 e) (locals @ [e]))), st)
                                      end
                              in
                                  case e of
                                      EJavaScript (m, orig as (EAbs (_, t, _, e), _), _) => doCode m 1 (t :: env) orig e
                                    | EJavaScript (m, e, _) => doCode m 0 env e e
                                    | _ => (e, st)
                              end,
                     decl = fn (_, e, st) => (e, st),
                     bind = fn (env, U.Decl.RelE (_, t)) => t :: env
                             | (env, _) => env}
                    []

fun process file =
    let
        fun doDecl (d, st) =
            let
                val (d, st) = decl st d
            in
                (List.revAppend (#decls st, [d]),
                 {decls = [],
                  script = #script st})
            end

        val (ds, st) = ListUtil.foldlMapConcat doDecl
                       {decls = [],
                        script = ""}
                       file

        val inf = TextIO.openIn (OS.Path.joinDirFile {dir = Config.libJs, file = "urweb.js"})
        fun lines acc =
            case TextIO.inputLine inf of
                NONE => String.concat (rev acc)
              | SOME line => lines (line :: acc)
        val lines = lines []
    in
        TextIO.closeIn inf;
        (DJavaScript lines, ErrorMsg.dummySpan) :: ds
    end

end
