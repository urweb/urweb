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

structure IS = IntBinarySet
structure IM = IntBinaryMap

val funcs = [(("Basis", "alert"), "alert"),
             (("Basis", "get_client_source"), "sg"),
             (("Basis", "htmlifyBool"), "bs"),
             (("Basis", "htmlifyFloat"), "ts"),
             (("Basis", "htmlifyInt"), "ts"),
             (("Basis", "htmlifyString"), "eh"),
             (("Basis", "new_client_source"), "sc"),
             (("Basis", "set_client_source"), "sv"),
             (("Basis", "stringToFloat_error"), "pfl"),
             (("Basis", "stringToInt_error"), "pi"),
             (("Basis", "urlifyInt"), "ts"),
             (("Basis", "urlifyFloat"), "ts"),
             (("Basis", "urlifyString"), "uf"),
             (("Basis", "recv"), "rv"),
             (("Basis", "strcat"), "cat"),
             (("Basis", "intToString"), "ts"),
             (("Basis", "floatToString"), "ts"),
             (("Basis", "onError"), "onError")]

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
     script : string list,
     included : IS.set,
     injectors : int IM.map,
     decoders : int IM.map,
     maxName : int
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
      | EServerCall (e, ek, _) => Int.max (varDepth e, varDepth ek)
      | ERecv (e, ek, _) => Int.max (varDepth e, varDepth ek)
      | ESleep (e, ek) => Int.max (varDepth e, varDepth ek)

fun closedUpto d =
    let
        fun cu inner (e, _) =
            case e of
                EPrim _ => true
              | ERel n => n < inner orelse n - inner >= d
              | ENamed _ => true
              | ECon (_, _, NONE) => true
              | ECon (_, _, SOME e) => cu inner e
              | ENone _ => true
              | ESome (_, e) => cu inner e
              | EFfi _ => true
              | EFfiApp (_, _, es) => List.all (cu inner) es
              | EApp (e1, e2) => cu inner e1 andalso cu inner e2
              | EAbs (_, _, _, e) => cu (inner + 1) e
              | EUnop (_, e) => cu inner e
              | EBinop (_, e1, e2) => cu inner e1 andalso cu inner e2
              | ERecord xes => List.all (fn (_, e, _) => cu inner e) xes
              | EField (e, _) => cu inner e
              | ECase (e, pes, _) =>
                cu inner e
                andalso List.all (fn (p, e) => cu (inner + E.patBindsN p) e) pes
              | EStrcat (e1, e2) => cu inner e1 andalso cu inner e2
              | EError (e, _) => cu inner e
              | EWrite e => cu inner e
              | ESeq (e1, e2) => cu inner e1 andalso cu inner e2
              | ELet (_, _, e1, e2) => cu inner e1 andalso cu (inner + 1) e2
              | EClosure (_, es) => List.all (cu inner) es
              | EQuery {query, body, initial, ...} =>
                cu inner query
                andalso cu (inner + 2) body
                andalso cu inner initial
              | EDml e => cu inner e
              | ENextval e => cu inner e
              | EUnurlify (e, _) => cu inner e
              | EJavaScript (_, e, _) => cu inner e
              | ESignalReturn e => cu inner e
              | ESignalBind (e1, e2) => cu inner e1 andalso cu inner e2
              | ESignalSource e => cu inner e
              | EServerCall (e, ek, _) => cu inner e andalso cu inner ek
              | ERecv (e, ek, _) => cu inner e andalso cu inner ek
              | ESleep (e, ek) => cu inner e andalso cu inner ek
    in
        cu 0
    end

fun strcat loc es =
    case es of
        [] => (EPrim (Prim.String ""), loc)
      | [x] => x
      | x :: es' => (EStrcat (x, strcat loc es'), loc)

fun process file =
    let
        val (someTs, nameds) =
            foldl (fn ((DVal (_, n, t, e, _), _), (someTs, nameds)) => (someTs, IM.insert (nameds, n, e))
                    | ((DValRec vis, _), (someTs, nameds)) =>
                      (someTs, foldl (fn ((_, n, _, e, _), nameds) => IM.insert (nameds, n, e))
                                     nameds vis)
                    | ((DDatatype (_, _, cs), _), state as (someTs, nameds)) =>
                      if ElabUtil.classifyDatatype cs = Option then
                          (foldl (fn ((_, n, SOME t), someTs) => IM.insert (someTs, n, t)
                                   | (_, someTs) => someTs) someTs cs,
                           nameds)
                      else
                          state
                    | (_, state) => state)
                  (IM.empty, IM.empty) file

        fun str loc s = (EPrim (Prim.String s), loc)

        fun isNullable (t, _) =
            case t of
                TOption _ => true
              | TRecord [] => true
              | _ => false

        fun quoteExp loc (t : typ) (e, st) =
            case #1 t of
                TSource => (strcat loc [str loc "s",
                                        (EFfiApp ("Basis", "htmlifyInt", [e]), loc)], st)

              | TRecord [] => (str loc "null", st)
              | TRecord [(x, t)] =>
                let
                    val (e, st) = quoteExp loc t ((EField (e, x), loc), st)
                in
                    (strcat loc [str loc ("{_" ^ x ^ ":"),
                                 e,
                                 str loc "}"], st)
                end
              | TRecord ((x, t) :: xts) =>
                let
                    val (e', st) = quoteExp loc t ((EField (e, x), loc), st)
                    val (es, st) = ListUtil.foldlMap
                                   (fn ((x, t), st) =>
                                       let
                                           val (e, st) = quoteExp loc t ((EField (e, x), loc), st)
                                       in
                                           (strcat loc [str loc (",_" ^ x ^ ":"), e], st)
                                       end)
                                   st xts
                in
                    (strcat loc (str loc ("{_" ^ x ^ ":")
                                 :: e'
                                 :: es
                                 @ [str loc "}"]), st)
                end

              | TFfi ("Basis", "string") => ((EFfiApp ("Basis", "jsifyString", [e]), loc), st)
              | TFfi ("Basis", "int") => ((EFfiApp ("Basis", "htmlifyInt", [e]), loc), st)
              | TFfi ("Basis", "float") => ((EFfiApp ("Basis", "htmlifyFloat", [e]), loc), st)
              | TFfi ("Basis", "channel") => ((EFfiApp ("Basis", "jsifyChannel", [e]), loc), st)

              | TFfi ("Basis", "bool") => ((ECase (e,
                                                   [((PCon (Enum, PConFfi {mod = "Basis",
                                                                           datatyp = "bool",
                                                                           con = "True",
                                                                           arg = NONE}, NONE), loc),
                                                     str loc "true"),
                                                    ((PCon (Enum, PConFfi {mod = "Basis",
                                                                           datatyp = "bool",
                                                                           con = "False",
                                                                           arg = NONE}, NONE), loc),
                                                     str loc "false")],
                                                   {disc = (TFfi ("Basis", "bool"), loc),
                                                    result = (TFfi ("Basis", "string"), loc)}), loc),
                                           st)

              | TOption t =>
                let
                    val (e', st) = quoteExp loc t ((ERel 0, loc), st)
                in
                    ((ECase (e,
                             [((PNone t, loc),
                               str loc "null"),
                              ((PSome (t, (PVar ("x", t), loc)), loc),
                               if isNullable t then
                                   strcat loc [str loc "{v:", e', str loc "}"]
                               else
                                   e')],
                             {disc = (TOption t, loc),
                              result = (TFfi ("Basis", "string"), loc)}), loc),
                     st)
                end

              | TDatatype (n, ref (dk, cs)) =>
                (case IM.find (#injectors st, n) of
                     SOME n' => ((EApp ((ENamed n', loc), e), loc), st)
                   | NONE =>
                     let
                         val n' = #maxName st
                         val st = {decls = #decls st,
                                   script = #script st,
                                   included = #included st,
                                   injectors = IM.insert (#injectors st, n, n'),
                                   decoders = #decoders st,
                                   maxName = n' + 1}

                         val (pes, st) = ListUtil.foldlMap
                                             (fn ((_, cn, NONE), st) =>
                                                 (((PCon (dk, PConVar cn, NONE), loc),
                                                   case dk of
                                                       Option => str loc "null"
                                                     | _ => str loc (Int.toString cn)),
                                                  st)
                                               | ((_, cn, SOME t), st) =>
                                                 let
                                                     val (e, st) = quoteExp loc t ((ERel 0, loc), st)
                                                 in
                                                     (((PCon (dk, PConVar cn, SOME (PVar ("x", t), loc)), loc),
                                                       case dk of
                                                           Option =>
                                                           if isNullable t then
                                                               strcat loc [str loc "{v:",
                                                                           e,
                                                                           str loc "}"]
                                                           else
                                                               e
                                                         | _ => strcat loc [str loc ("{n:" ^ Int.toString cn
                                                                                     ^ ",v:"),
                                                                            e,
                                                                            str loc "}"]),
                                                      st)
                                                 end)
                                             st cs

                         val s = (TFfi ("Basis", "string"), loc)
                         val body = (ECase ((ERel 0, loc), pes,
                                            {disc = t, result = s}), loc)
                         val body = (EAbs ("x", t, s, body), loc)

                         val st = {decls = (DValRec [("jsify", n', (TFun (t, s), loc),
                                                      body, "jsify")], loc) :: #decls st,
                                   script = #script st,
                                   included = #included st,
                                   injectors = #injectors st,
                                   decoders= #decoders st,
                                   maxName = #maxName st}
                     in
                         ((EApp ((ENamed n', loc), e), loc), st)
                     end)

              | _ => (EM.errorAt loc "Don't know how to embed type in JavaScript";
                      Print.prefaces "Can't embed" [("t", MonoPrint.p_typ MonoEnv.empty t)];
                      (str loc "ERROR", st))

        fun unurlifyExp loc (t : typ, st) =
            case #1 t of
                TRecord [] => ("null", st)
              | TRecord [(x, t)] =>
                let
                    val (e, st) = unurlifyExp loc (t, st)
                in
                    ("{_" ^ x ^ ":" ^ e ^ "}",
                     st)
                end
              | TRecord ((x, t) :: xts) =>
                let
                    val (e', st) = unurlifyExp loc (t, st)
                    val (es, st) = ListUtil.foldlMap
                                       (fn ((x, t), st) =>
                                           let
                                               val (e, st) = unurlifyExp loc (t, st)
                                           in
                                               (",_" ^ x ^ ":" ^ e, st)
                                           end)
                                       st xts
                in
                    (String.concat ("{_"
                                    :: x
                                    :: ":"
                                    :: e'
                                    :: es
                                    @ ["}"]), st)
                end

              | TFfi ("Basis", "string") => ("uu(t[i++])", st)
              | TFfi ("Basis", "int") => ("parseInt(t[i++])", st)
              | TFfi ("Basis", "float") => ("parseFloat(t[i++])", st)
              | TFfi ("Basis", "channel") => ("(t[i++].length > 0 ? parseInt(t[i]) : null)", st)

              | TFfi ("Basis", "bool") => ("t[i++] == \"True\"", st)

              | TOption t =>
                let
                    val (e, st) = unurlifyExp loc (t, st)
                    val e = if isNullable t then
                                "{v:" ^ e ^ "}"
                            else
                                e
                in
                    ("(t[i++]==\"Some\"?" ^ e ^ ":null)", st)
                end

              | TDatatype (n, ref (dk, cs)) =>
                (case IM.find (#decoders st, n) of
                     SOME n' => ("(tmp=_n" ^ Int.toString n' ^ "(t,i),i=tmp._1,tmp._2)", st)
                   | NONE =>
                     let
                         val n' = #maxName st
                         val st = {decls = #decls st,
                                   script = #script st,
                                   included = #included st,
                                   injectors = #injectors st,
                                   decoders = IM.insert (#decoders st, n, n'),
                                   maxName = n' + 1}

                         val (e, st) = foldl (fn ((x, cn, NONE), (e, st)) =>
                                                 ("x==\"" ^ x ^ "\"?"
                                                   ^ (case dk of
                                                          Option => "null"
                                                        | _ => Int.toString cn)
                                                  ^ ":" ^ e,
                                                  st)
                                               | ((x, cn, SOME t), (e, st)) =>
                                                 let
                                                     val (e', st) = unurlifyExp loc (t, st)
                                                 in
                                                     ("x==\"" ^ x ^ "\"?"
                                                       ^ (case dk of
                                                              Option =>
                                                              if isNullable t then
                                                                  "{v:" ^ e' ^ "}"
                                                              else
                                                                  e'
                                                            | _ => "{n:" ^ Int.toString cn ^ ",v:" ^ e' ^ "}")
                                                      ^ ":" ^ e,
                                                      st)
                                                 end)
                                             ("pf()", st) cs

                         val body = "function _n" ^ Int.toString n' ^ "(t,i){var x=t[i++];var r="
                                    ^ e ^ ";return {_1:i,_2:r}}\n\n"

                         val st = {decls = #decls st,
                                   script = body :: #script st,
                                   included = #included st,
                                   injectors = #injectors st,
                                   decoders = #decoders st,
                                   maxName = #maxName st}
                     in
                         ("(tmp=_n" ^ Int.toString n' ^ "(t,i),i=tmp._1,tmp._2)", st)
                     end)

              | _ => (EM.errorAt loc "Don't know how to unurlify type in JavaScript";
                      Print.prefaces "Can't unurlify" [("t", MonoPrint.p_typ MonoEnv.empty t)];
                      ("ERROR", st))

        fun padWith (ch, s, len) =
            if size s < len then
                padWith (ch, String.str ch ^ s, len - 1)
            else
                s

        fun jsExp mode skip outer =
            let
                val len = length outer

                fun jsE inner (e as (_, loc), st) =
                    let
                        val str = str loc

                        fun var n = Int.toString (len + inner - n - 1)

                        fun patCon pc =
                            case pc of
                                PConVar n => str (Int.toString n)
                              | PConFfi {mod = "Basis", con = "True", ...} => str "true"
                              | PConFfi {mod = "Basis", con = "False", ...} => str "false"
                              | PConFfi {con, ...} => str ("\"_" ^ con ^ "\"")

                        fun unsupported s =
                            (EM.errorAt loc (s ^ " in code to be compiled to JavaScript[2]");
                             (str "ERROR", st))

                        val strcat = strcat loc

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
                                                          | #"\n" => "\\n"
                                                          | #"\r" => "\\r"
                                                          | #"\t" => "\\t"
                                                          | ch =>
                                                            if Char.isPrint ch then
                                                                String.str ch
                                                            else
                                                                "\\" ^ padWith (#"0",
                                                                                Int.fmt StringCvt.OCT (ord ch),
                                                                                3)) s
                                     ^ "\"")
                              | _ => str (Prim.toString p)

                        fun jsPat depth inner (p, _) succ fail =
                            case p of
                                PWild => succ
                              | PVar _ => strcat [str ("(_" ^ Int.toString (len + inner) ^ "=d"
                                                       ^ Int.toString depth ^ ","),
                                                  succ,
                                                  str ")"]
                              | PPrim p => strcat [str ("(d" ^ Int.toString depth ^ "=="),
                                                   jsPrim p,
                                                   str "?",
                                                   succ,
                                                   str ":",
                                                   fail,
                                                   str ")"]
                              | PCon (_, PConFfi {mod = "Basis", con = "True", ...}, NONE) =>
                                strcat [str ("(d" ^ Int.toString depth ^ "?"),
                                        succ,
                                        str ":",
                                        fail,
                                        str ")"]
                              | PCon (_, PConFfi {mod = "Basis", con = "False", ...}, NONE) =>
                                strcat [str ("(d" ^ Int.toString depth ^ "?"),
                                        fail,
                                        str ":",
                                        succ,
                                        str ")"]
                              | PCon (Option, _, NONE) =>
                                strcat [str ("(d" ^ Int.toString depth ^ "?"),
                                        fail,
                                        str ":",
                                        succ,
                                        str ")"]
                              | PCon (Option, PConVar n, SOME p) =>
                                (case IM.find (someTs, n) of
                                     NONE => raise Fail "Jscomp: Not in someTs"
                                   | SOME t =>
                                     strcat [str ("(d" ^ Int.toString depth ^ "?("
                                                  ^ (if isNullable t then
                                                         "d" ^ Int.toString depth ^ "=d"
                                                         ^ Int.toString depth ^ ".v,"
                                                     else
                                                         "")),
                                             jsPat depth inner p succ fail,
                                             str "):",
                                             fail,
                                             str ")"])
                              | PCon (_, pc, NONE) =>
                                strcat [str ("(d" ^ Int.toString depth ^ "=="),
                                        patCon pc,
                                        str "?",
                                        succ,
                                        str ":",
                                        fail,
                                        str ")"]
                              | PCon (_, pc, SOME p) =>
                                strcat [str ("(d" ^ Int.toString depth ^ ".n=="),
                                        patCon pc,
                                        str ("?(d" ^ Int.toString depth ^ "=d" ^ Int.toString depth ^ ".v,"),
                                        jsPat depth inner p succ fail,
                                        str "):",
                                        fail,
                                        str ")"]
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
                              | PSome (t, p) => strcat (str ("(d" ^ Int.toString depth ^ "?")
                                                        :: (if isNullable t then
                                                                [str ("d" ^ Int.toString depth
                                                                      ^ "=d" ^ Int.toString depth ^ ".v")]
                                                            else
                                                                [])
                                                        @ [jsPat depth inner p succ fail,
                                                           str ":",
                                                           fail,
                                                           str ")"])

                        val jsifyString = String.translate (fn #"\"" => "\\\""
                                                             | #"\\" => "\\\\"
                                                             | ch => String.str ch)

                        fun jsifyStringMulti (n, s) =
                            case n of
                                0 => s
                              | _ => jsifyStringMulti (n - 1, jsifyString s)

                        fun deStrcat level (all as (e, _)) =
                            case e of
                                EPrim (Prim.String s) => jsifyStringMulti (level, s)
                              | EStrcat (e1, e2) => deStrcat level e1 ^ deStrcat level e2
                              | EFfiApp ("Basis", "jsifyString", [e]) => "\"" ^ deStrcat (level + 1) e ^ "\""
                              | _ => (Print.prefaces "deStrcat" [("e", MonoPrint.p_exp MonoEnv.empty all)];
                                      raise Fail "Jscomp: deStrcat")

                        val quoteExp = quoteExp loc
                    in
                        (*Print.prefaces "jsE" [("e", MonoPrint.p_exp MonoEnv.empty e)];*)

                        case #1 e of
                            EPrim p => (jsPrim p, st)
                          | ERel n =>
                            if n < inner then
                                (str ("_" ^ var n), st)
                            else
                                let
                                    val n = n - inner
                                in
                                    quoteExp (List.nth (outer, n)) ((ERel (n - skip), loc), st)
                                end

                          | ENamed n =>
                            let
                                val st =
                                    if IS.member (#included st, n) then
                                        st
                                    else
                                        case IM.find (nameds, n) of
                                            NONE => raise Fail "Jscomp: Unbound ENamed"
                                          | SOME e =>
                                            let
                                                val st = {decls = #decls st,
                                                          script = #script st,
                                                          included = IS.add (#included st, n),
                                                          injectors = #injectors st,
                                                          decoders = #decoders st,
                                                          maxName = #maxName st}

                                                val (e, st) = jsExp mode skip [] 0 (e, st)
                                                val e = deStrcat 0 e
                                                
                                                val sc = "_n" ^ Int.toString n ^ "=" ^ e ^ ";\n"
                                            in
                                                {decls = #decls st,
                                                 script = sc :: #script st,
                                                 included = #included st,
                                                 injectors = #injectors st,
                                                 decoders= #decoders st,
                                                 maxName = #maxName st}
                                            end
                            in
                                (str ("_n" ^ Int.toString n), st)
                            end

                          | ECon (Option, _, NONE) => (str "null", st)
                          | ECon (Option, PConVar n, SOME e) =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                case IM.find (someTs, n) of
                                    NONE => raise Fail "Jscomp: Not in someTs [2]"
                                  | SOME t =>
                                    (if isNullable t then
                                         strcat [str "{v:",
                                                 e,
                                                 str "}"]
                                     else
                                         e, st)
                            end

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
                                               NONE => (EM.errorAt loc ("Unsupported FFI identifier " ^ #2 k
                                                                        ^ " in JavaScript");
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
                                               NONE => (EM.errorAt loc ("Unsupported FFI function "
                                                                        ^ x ^ " in JavaScript");
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

                          | ECase (e', pes, {result, ...}) =>
                            (*if closedUpto inner e andalso List.all (fn (_, e) => closedUpto inner e) pes then
                                let
                                    val (e', st) = quoteExp result ((ERel 0, loc), st)
                                in
                                    ((ELet ("js", result, e, e'), loc),
                                     st)
                                end
                            else*)
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

                                    val (e, st) = jsE inner (e', st)
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
                                (strcat [str "cat(", e1, str ",", e2, str ")"], st)
                            end

                          | EError (e, _) =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str "er(", e, str ")"],
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

                          | EJavaScript (Source _, _, SOME _) => (e, st)
                          | EJavaScript (_, _, SOME e) =>
                            (strcat [str "cs(function(){return ",
                                     e,
                                     str "})"],
                             st)

                          | EClosure _ => unsupported "EClosure"
                          | EQuery _ => unsupported "Query"
                          | EDml _ => unsupported "DML"
                          | ENextval _ => unsupported "Nextval"
                          | EUnurlify _ => unsupported "EUnurlify"
                          | EJavaScript (_, e, _) =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str "cs(function(){return ",
                                         e,
                                         str "})"],
                                 st)
                            end

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

                          | EServerCall (e, ek, t) =>
                            let
                                val (e, st) = jsE inner (e, st)
                                val (ek, st) = jsE inner (ek, st)
                                val (unurl, st) = unurlifyExp loc (t, st)
                            in
                                (strcat [str ("rc(cat(\"" ^ !Monoize.urlPrefix ^ "\","),
                                         e,
                                         str ("), function(s){var t=s.split(\"/\");var i=0;return "
                                              ^ unurl ^ "},"),
                                         ek,
                                         str ")"],
                                 st)
                            end

                          | ERecv (e, ek, t) =>
                            let
                                val (e, st) = jsE inner (e, st)
                                val (ek, st) = jsE inner (ek, st)
                                val (unurl, st) = unurlifyExp loc (t, st)
                            in
                                (strcat [str "rv(",
                                         e,
                                         str (", function(s){var t=s.split(\"/\");var i=0;return "
                                              ^ unurl ^ "},"),
                                         ek,
                                         str ")"],
                                 st)
                            end

                          | ESleep (e, ek) =>
                            let
                                val (e, st) = jsE inner (e, st)
                                val (ek, st) = jsE inner (ek, st)
                            in
                                (strcat [str "window.setTimeout(",
                                         ek,
                                         str ", ",
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
                                              EJavaScript (m, orig, NONE) =>
                                              doCode m 0 env orig orig
                                            | _ => (e, st)
                                      end,
                             decl = fn (_, e, st) => (e, st),
                             bind = fn (env, U.Decl.RelE (_, t)) => t :: env
                                     | (env, _) => env}
                            []

        fun doDecl (d, st) =
            let
                val (d, st) = decl st d
            in
                (List.revAppend (#decls st, [d]),
                 {decls = [],
                  script = #script st,
                  included = #included st,
                  injectors = #injectors st,
                  decoders = #decoders st,
                  maxName = #maxName st})
            end

        val (ds, st) = ListUtil.foldlMapConcat doDecl
                       {decls = [],
                        script = [],
                        included = IS.empty,
                        injectors = IM.empty,
                        decoders = IM.empty,
                        maxName = U.File.maxName file + 1}
                       file

        val inf = TextIO.openIn (OS.Path.joinDirFile {dir = Config.libJs, file = "urweb.js"})
        fun lines acc =
            case TextIO.inputLine inf of
                NONE => String.concat (rev acc)
              | SOME line => lines (line :: acc)
        val lines = lines []
    in
        TextIO.closeIn inf;
        (DJavaScript (lines ^ String.concat (rev (#script st))), ErrorMsg.dummySpan) :: ds
    end

end
