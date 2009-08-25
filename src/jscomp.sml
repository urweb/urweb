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

structure TM = BinaryMapFn(struct
                           type ord_key = typ
                           val compare = U.Typ.compare
                           end)

type state = {
     decls : (string * int * (string * int * typ option) list) list,
     script : string list,
     included : IS.set,
     injectors : int IM.map,
     listInjectors : int TM.map,
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
      | EReturnBlob {blob = e1, mimeType = e2, ...} => Int.max (varDepth e1, varDepth e2)
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
      | EServerCall (e, ek, _, _) => Int.max (varDepth e, varDepth ek)
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
              | EReturnBlob {blob = e1, mimeType = e2, ...} => cu inner e1 andalso cu inner e2
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
              | EJavaScript (_, e) => cu inner e
              | ESignalReturn e => cu inner e
              | ESignalBind (e1, e2) => cu inner e1 andalso cu inner e2
              | ESignalSource e => cu inner e
              | EServerCall (e, ek, _, _) => cu inner e andalso cu inner ek
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

fun patDepth (p, _) =
    case p of
        PWild => 0
      | PVar _ => 0
      | PPrim _ => 0
      | PCon (_, _, NONE) => 0
      | PCon (_, _, SOME p) => 1 + patDepth p
      | PRecord xpts => foldl Int.max 0 (map (fn (_, p, _) => 1 + patDepth p) xpts)
      | PNone _ => 0
      | PSome (_, p) => 1 + patDepth p

val compact =
    U.Exp.mapB {typ = fn t => t,
                exp = fn inner => fn e =>
                                     case e of
                                         ERel n =>
                                         if n >= inner then
                                             ERel (n - inner)
                                         else
                                             e
                                       | _ => e,
                bind = fn (inner, b) =>
                          case b of
                              U.Exp.RelE _ => inner+1
                            | _ => inner}

exception CantEmbed of typ

fun inString {needle, haystack} =
    let
        val (_, suffix) = Substring.position needle (Substring.full haystack)
    in
        not (Substring.isEmpty suffix)
    end

fun process file =
    let
        val (someTs, nameds) =
            foldl (fn ((DVal (_, n, t, e, _), _), (someTs, nameds)) => (someTs, IM.insert (nameds, n, e))
                    | ((DValRec vis, _), (someTs, nameds)) =>
                      (someTs, foldl (fn ((_, n, _, e, _), nameds) => IM.insert (nameds, n, e))
                                     nameds vis)
                    | ((DDatatype dts, _), state as (someTs, nameds)) =>
                      (foldl (fn ((_, _, cs), someTs) =>
                                 if ElabUtil.classifyDatatype cs = Option then
                                     foldl (fn ((_, n, SOME t), someTs) => IM.insert (someTs, n, t)
                                             | (_, someTs) => someTs) someTs cs
                                 else
                                     someTs) someTs dts,
                       nameds)
                    | (_, state) => state)
                  (IM.empty, IM.empty) file

        fun str loc s = (EPrim (Prim.String s), loc)

        fun isNullable (t, _) =
            case t of
                TOption _ => true
              | TList _ => true
              | TDatatype (_, ref (Option, _)) => true
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
                    (case #1 e' of
                        EPrim (Prim.String "ERROR") => raise Fail "UHOH"
                      | _ =>
                        (ECase (e,
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

              | TList t' =>
                (case TM.find (#listInjectors st, t') of
                     SOME n' => ((EApp ((ENamed n', loc), e), loc), st)
                   | NONE =>
                     let
                         val rt = (TRecord [("1", t'), ("2", t)], loc)

                         val n' = #maxName st
                         val st = {decls = #decls st,
                                   script = #script st,
                                   included = #included st,
                                   injectors = #injectors st,
                                   listInjectors = TM.insert (#listInjectors st, t', n'),
                                   decoders = #decoders st,
                                   maxName = n' + 1}

                         val s = (TFfi ("Basis", "string"), loc)
                         val (e', st) = quoteExp loc t' ((EField ((ERel 0, loc), "1"), loc), st)

                         val body = (ECase ((ERel 0, loc),
                                            [((PNone rt, loc),
                                              str loc "null"),
                                             ((PSome (rt, (PVar ("x", rt), loc)), loc),
                                              strcat loc [str loc ((if isNullable t' then
                                                                        "{v:"
                                                                    else
                                                                        "") ^ "{_1:"),
                                                          e',
                                                          str loc ",_2:",
                                                          (EApp ((ENamed n', loc),
                                                                 (EField ((ERel 0, loc), "2"), loc)), loc),
                                                          str loc ((if isNullable t' then
                                                                        "}"
                                                                    else
                                                                        "") ^ "}")])],
                                            {disc = t, result = s}), loc)
                         val body = (EAbs ("x", t, s, body), loc)
                                    
                         val st = {decls = ("jsify", n', (TFun (t, s), loc),
                                            body, "jsify") :: #decls st,
                                   script = #script st,
                                   included = #included st,
                                   injectors = #injectors st,
                                   listInjectors = #listInjectors st,
                                   decoders= #decoders st,
                                   maxName = #maxName st}


                     in
                         ((EApp ((ENamed n', loc), e), loc), st)
                     end)

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
                                   listInjectors = #listInjectors st,
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

                         val st = {decls = ("jsify", n', (TFun (t, s), loc),
                                            body, "jsify") :: #decls st,
                                   script = #script st,
                                   included = #included st,
                                   injectors = #injectors st,
                                   listInjectors = #listInjectors st,
                                   decoders= #decoders st,
                                   maxName = #maxName st}
                     in
                         ((EApp ((ENamed n', loc), e), loc), st)
                     end)

              | _ => ((*Print.prefaces "Can't embed" [("t", MonoPrint.p_typ MonoEnv.empty t)];*)
                      raise CantEmbed t)

        fun unurlifyExp loc (t : typ, st) =
            case #1 t of
                TRecord [] => ("null", st)
              | TFfi ("Basis", "unit") => ("null", st)
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

              | TFfi ("Basis", "bool") => ("t[i++] == \"1\"", st)

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

              | TList t =>
                let
                    val (e, st) = unurlifyExp loc (t, st)
                in
                    ("uul(function(){return t[i++];},function(){return " ^ e ^ "})", st)
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
                                   listInjectors = #listInjectors st,
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
                                             ("pf(\"" ^ ErrorMsg.spanToString loc ^ "\")", st) cs

                         val body = "function _n" ^ Int.toString n' ^ "(t,i){var x=t[i++];var r="
                                    ^ e ^ ";return {_1:i,_2:r}}\n\n"

                         val st = {decls = #decls st,
                                   script = body :: #script st,
                                   included = #included st,
                                   injectors = #injectors st,
                                   listInjectors = #listInjectors st,
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

        val foundJavaScript = ref false

        fun jsExp mode outer =
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
                             Print.preface ("Code", MonoPrint.p_exp MonoEnv.empty e);
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
                                                            (*if mode = Script then
                                                                "<"
                                                            else*)
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
                              | Prim.Char ch => str ("'" ^ String.str ch ^ "'")
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
                                strcat [str ("(d" ^ Int.toString depth ^ "!=null?"),
                                        fail,
                                        str ":",
                                        succ,
                                        str ")"]
                              | PCon (Option, PConVar n, SOME p) =>
                                (case IM.find (someTs, n) of
                                     NONE => raise Fail "Jscomp: Not in someTs"
                                   | SOME t =>
                                     strcat [str ("(d" ^ Int.toString depth ^ "!=null?(d"
                                                  ^ Int.toString (depth+1) ^ "=d" ^ Int.toString depth
                                                  ^ (if isNullable t then
                                                         ".v,"
                                                     else
                                                         "")
                                                  ^ ","),
                                             jsPat (depth+1) inner p succ fail,
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
                                        str ("?(d" ^ Int.toString (depth+1) ^ "=d" ^ Int.toString depth ^ ".v,"),
                                        jsPat (depth+1) inner p succ fail,
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
                              | PNone _ => strcat [str ("(d" ^ Int.toString depth ^ "!=null?"),
                                                   fail,
                                                   str ":",
                                                   succ,
                                                   str ")"]
                              | PSome (t, p) => strcat [str ("(d" ^ Int.toString depth ^ "!=null?(d" ^ Int.toString (depth+1)
                                                             ^ "=d" ^ Int.toString depth
                                                             ^ (if isNullable t then
                                                                    ".v"
                                                                else
                                                                    "")
                                                             ^ ","),
                                                        jsPat (depth+1) inner p succ fail,
                                                        str "):",
                                                        fail,
                                                        str ")"]

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

                        val hasQuery = U.Exp.exists {typ = fn _ => false,
                                                     exp = fn EQuery _ => true
                                                            | _ => false}

                        val indirectQuery = U.Exp.exists {typ = fn _ => false,
                                                          exp = fn ENamed n =>
                                                                   (case IM.find (nameds, n) of
                                                                        NONE => false
                                                                      | SOME e => hasQuery e)
                                                                 | _ => false}

                    in
                        (*if indirectQuery e then
                            Print.preface ("Indirect", MonoPrint.p_exp MonoEnv.empty e)
                        else
                            ();*)

                        (*Print.prefaces "jsE" [("e", MonoPrint.p_exp MonoEnv.empty e),
                                              ("inner", Print.PD.string (Int.toString inner))];*)

                        case #1 e of
                            EPrim p => (jsPrim p, st)
                          | ERel n =>
                            if n < inner then
                                (str ("_" ^ var n), st)
                            else
                                let
                                    val n = n - inner
                                    (*val () = Print.prefaces "quote" [("t", MonoPrint.p_typ MonoEnv.empty
                                                                           (List.nth (outer, n)))]*)
                                in
                                    quoteExp (List.nth (outer, n)) ((ERel n, loc), st)
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
                                                          listInjectors = #listInjectors st,
                                                          decoders = #decoders st,
                                                          maxName = #maxName st}

                                                val old = e
                                                val (e, st) = jsExp mode [] 0 (e, st)
                                                val new = e
                                                val e = deStrcat 0 e
                                                
                                                val sc = "_n" ^ Int.toString n ^ "=" ^ e ^ ";\n"
                                            in
                                                (*Print.prefaces "jsify'" [("old", MonoPrint.p_exp MonoEnv.empty old),
                                                                         ("new", MonoPrint.p_exp MonoEnv.empty new)];*)
                                                {decls = #decls st,
                                                 script = sc :: #script st,
                                                 included = #included st,
                                                 injectors = #injectors st,
                                                 listInjectors = #listInjectors st,
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
                                val name = case Settings.jsFunc k of
                                               NONE => (EM.errorAt loc ("Unsupported FFI identifier " ^ #2 k
                                                                        ^ " in JavaScript");
                                                        "ERROR")
                                             | SOME s => s
                            in
                                (str name, st)
                            end
                          | EFfiApp ("Basis", "kc", []) => (str "kc(event)", st)
                          | EFfiApp (m, x, args) =>
                            let
                                val name = case Settings.jsFunc (m, x) of
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
                                val s =
                                    case s of
                                        "!strcmp" => "=="
                                      | _ => s

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
                                (strcat [str ("{_" ^ x ^ ":"), e, str "}"], st)
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
                            let
                                val plen = length pes

                                val (cases, st) = ListUtil.foldliMap
                                                      (fn (i, (p, e), st) =>
                                                          let
                                                              val (e, st) = jsE (inner + E.patBindsN p) (e, st)
                                                              val fail =
                                                                  if i = plen - 1 then
                                                                      str ("pf(\"" ^ ErrorMsg.spanToString loc ^ "\")")
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

                                val depth = foldl Int.max 0 (map (fn (p, _) => 1 + patDepth p) pes)
                                val normalDepth = foldl Int.max 0 (map (fn (_, e) => 1 + varDepth e) pes)
                                val (e, st) = jsE inner (e', st)

                                val len = inner + len
                                val normalVars = List.tabulate (normalDepth, fn n => "_" ^ Int.toString (n + len))
                                val patVars = List.tabulate (depth, fn n => "d" ^ Int.toString n)
                                val caseVars = ListUtil.mapi (fn (i, _) => "c" ^ Int.toString i) pes
                            in
                                (strcat (str "(function (){ var "
                                         :: str (String.concatWith "," (normalVars @ patVars @ caseVars) ^ ";d0=")
                                         :: e
                                         :: str ";\nreturn ("
                                         :: List.revAppend (cases,
                                                            [str "c0()) } ())"])), st)
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

                          | EJavaScript (Source _, e) =>
                            (foundJavaScript := true;
                             jsE inner (e, st))
                          | EJavaScript (_, e) =>
                            let
                                val locals = List.tabulate
                                                 (varDepth e,
                                               fn i => str ("var _" ^ Int.toString (len + inner + i) ^ ";"))

                                val (e, st) = jsE inner (e, st)
                            in
                                foundJavaScript := true;
                                (strcat (str "cs(function(){"
                                         :: locals
                                         @ [str "return ",
                                            compact inner e,
                                            str "})"]),
                                 st)
                            end

                          | EClosure _ => unsupported "EClosure"
                          | EQuery _ => unsupported "Query"
                          | EDml _ => unsupported "DML"
                          | ENextval _ => unsupported "Nextval"
                          | EUnurlify _ => unsupported "EUnurlify"
                          | EReturnBlob _ => unsupported "EUnurlify"

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

                          | EServerCall (e, ek, t, eff) =>
                            let
                                val (e, st) = jsE inner (e, st)
                                val (ek, st) = jsE inner (ek, st)
                                val (unurl, st) = unurlifyExp loc (t, st)
                            in
                                (strcat [str ("rc(cat(\"" ^ Settings.getUrlPrefix () ^ "\","),
                                         e,
                                         str ("), function(s){var t=s.split(\"/\");var i=0;return "
                                              ^ unurl ^ "},"),
                                         ek,
                                         str (","
                                              ^ (case eff of
                                                     ReadCookieWrite => "true"
                                                   | _ => "false")
                                              ^ ")")],
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


        fun patBinds ((p, _), env) =
            case p of
                PWild => env
              | PVar (_, t) => t :: env
              | PPrim _ => env
              | PCon (_, _, NONE) => env
              | PCon (_, _, SOME p) => patBinds (p, env)
              | PRecord xpts => foldl (fn ((_, p, _), env) => patBinds (p, env)) env xpts
              | PNone _ => env
              | PSome (_, p) => patBinds (p, env)

        fun exp outer (e as (_, loc), st) =
            ((*Print.preface ("exp", MonoPrint.p_exp MonoEnv.empty e);*)
             case #1 e of
                 EPrim p =>
                 (case p of
                      Prim.String s => if inString {needle = "<script", haystack = s} then
                                           foundJavaScript := true
                                       else
                                           ()
                    | _ => ();
                  (e, st))
               | ERel _ => (e, st)
               | ENamed _ => (e, st)
               | ECon (_, _, NONE) => (e, st)
               | ECon (dk, pc, SOME e) => 
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((ECon (dk, pc, SOME e), loc), st)
                 end
               | ENone _ => (e, st)
               | ESome (t, e) =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((ESome (t, e), loc), st)
                 end
               | EFfi _ => (e, st)
               | EFfiApp (m, x, es) =>
                 let
                     val (es, st) = ListUtil.foldlMap (exp outer) st es
                 in
                     ((EFfiApp (m, x, es), loc), st)
                 end
               | EApp (e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((EApp (e1, e2), loc), st)
                 end
               | EAbs (x, dom, ran, e) =>
                 let
                     val (e, st) = exp (dom :: outer) (e, st)
                 in
                     ((EAbs (x, dom, ran, e), loc), st)
                 end

               | EUnop (s, e) =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((EUnop (s, e), loc), st)
                 end
               | EBinop (s, e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((EBinop (s, e1, e2), loc), st)
                 end
                 
               | ERecord xets =>
                 let
                     val (xets, st) = ListUtil.foldlMap (fn ((x, e, t), st) =>
                                                            let
                                                                val (e, st) = exp outer (e, st)
                                                            in
                                                                ((x, e, t), st)
                                                            end) st xets
                 in
                     ((ERecord xets, loc), st)
                 end
               | EField (e, s) =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((EField (e, s), loc), st)
                 end

               | ECase (e, pes, ts) =>
                 let
                     val (e, st) = exp outer (e, st)
                     val (pes, st) = ListUtil.foldlMap (fn ((p, e), st) =>
                                                           let
                                                               val (e, st) = exp (patBinds (p, outer)) (e, st)
                                                           in
                                                               ((p, e), st)
                                                           end) st pes
                 in
                     ((ECase (e, pes, ts), loc), st)
                 end

               | EStrcat (e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((EStrcat (e1, e2), loc), st)
                 end

               | EError (e, t) =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((EError (e, t), loc), st)
                 end
               | EReturnBlob {blob, mimeType, t} =>
                 let
                     val (blob, st) = exp outer (blob, st)
                     val (mimeType, st) = exp outer (mimeType, st)
                 in
                     ((EReturnBlob {blob = blob, mimeType = mimeType, t = t}, loc), st)
                 end

               | EWrite e =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((EWrite e, loc), st)
                 end
               | ESeq (e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((ESeq (e1, e2), loc), st)
                 end
               | ELet (x, t, e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp (t :: outer) (e2, st)
                 in
                     ((ELet (x, t, e1, e2), loc), st)
                 end

               | EClosure (n, es) =>
                 let
                     val (es, st) = ListUtil.foldlMap (exp outer) st es
                 in
                     ((EClosure (n, es), loc), st)
                 end

               | EQuery {exps, tables, state, query, body, initial} =>
                 let
                     val (query, st) = exp outer (query, st)
                     val (body, st) = exp outer (body, st)
                     val (initial, st) = exp outer (initial, st)
                 in
                     ((EQuery {exps = exps, tables = tables, state = state,
                               query = query, body = body, initial = initial}, loc), st)
                 end
               | EDml e =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((EDml e, loc), st)
                 end
               | ENextval e =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((ENextval e, loc), st)
                 end

               | EUnurlify (e, t) =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((EUnurlify (e, t), loc), st)
                 end

               | EJavaScript (m, e') =>
                 (let
                      val len = length outer
                      fun str s = (EPrim (Prim.String s), #2 e')

                      val locals = List.tabulate
                                       (varDepth e',
                                     fn i => str ("var _" ^ Int.toString (len + i) ^ ";"))

                      val (e', st) = jsExp m outer 0 (e', st)

                      val e' =
                          case locals of
                              [] => e'
                            | _ =>
                              strcat (#2 e') (str "(function(){"
                                              :: locals
                                              @ [str "return ",
                                                 e',
                                                 str "}())"])
                  in
                      (e', st)
                  end handle CantEmbed _ => (e, st))

               | ESignalReturn e =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((ESignalReturn e, loc), st)
                 end
               | ESignalBind (e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((ESignalBind (e1, e2), loc), st)
                 end
               | ESignalSource e =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((ESignalSource e, loc), st)
                 end
                 
               | EServerCall (e1, e2, t, ef) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((EServerCall (e1, e2, t, ef), loc), st)
                 end
               | ERecv (e1, e2, t) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((ERecv (e1, e2, t), loc), st)
                 end
               | ESleep (e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((ESleep (e1, e2), loc), st)
                 end)

        fun decl (d as (_, loc), st) =
            case #1 d of
                DVal (x, n, t, e, s) =>
                let
                    val (e, st) = exp [] (e, st)
                in
                    ((DVal (x, n, t, e, s), loc), st)
                end
              | DValRec vis =>
                let
                    val (vis, st) = ListUtil.foldlMap (fn ((x, n, t, e, s), st) =>
                                                          let
                                                              val (e, st) = exp [] (e, st)
                                                          in
                                                              ((x, n, t, e, s), st)
                                                          end) st vis
                in
                    ((DValRec vis, loc), st)
                end
              | _ => (d, st)

        fun doDecl (d, st) =
            let
                (*val () = Print.preface ("doDecl", MonoPrint.p_decl MonoEnv.empty d)*)
                val (d, st) = decl (d, st)

                val ds =
                    case #decls st of
                        [] => [d]
                      | vis => [(DValRec vis, #2 d), d]
            in
                (ds,
                 {decls = [],
                  script = #script st,
                  included = #included st,
                  injectors = #injectors st,
                  listInjectors = #listInjectors st,
                  decoders = #decoders st,
                  maxName = #maxName st})
            end

        val (ds, st) = ListUtil.foldlMapConcat doDecl
                       {decls = [],
                        script = [],
                        included = IS.empty,
                        injectors = IM.empty,
                        listInjectors = TM.empty,
                        decoders = IM.empty,
                        maxName = U.File.maxName file + 1}
                       file

        val inf = TextIO.openIn (OS.Path.joinDirFile {dir = Config.libJs, file = "urweb.js"})
        fun lines acc =
            case TextIO.inputLine inf of
                NONE => String.concat (rev acc)
              | SOME line => lines (line :: acc)
        val lines = lines []

        val script =
            if !foundJavaScript then
                lines ^ String.concat (rev (#script st))
            else
                ""
    in
        TextIO.closeIn inf;
        (DJavaScript script, ErrorMsg.dummySpan) :: ds
    end

end
