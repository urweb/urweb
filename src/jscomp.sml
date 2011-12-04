(* Copyright (c) 2008-2011, Adam Chlipala
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

fun strcat loc es =
    case es of
        [] => (EPrim (Prim.String ""), loc)
      | [x] => x
      | x :: es' => (EStrcat (x, strcat loc es'), loc)

exception CantEmbed of typ

fun inString {needle, haystack} = String.isSubstring needle haystack

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
                TSource => ((EFfiApp ("Basis", "htmlifySource", [e]), loc), st)

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
              | TFfi ("Basis", "char") => ((EFfiApp ("Basis", "jsifyChar", [e]), loc), st)
              | TFfi ("Basis", "int") => ((EFfiApp ("Basis", "htmlifyInt", [e]), loc), st)
              | TFfi ("Basis", "float") => ((EFfiApp ("Basis", "htmlifyFloat", [e]), loc), st)
              | TFfi ("Basis", "channel") => ((EFfiApp ("Basis", "jsifyChannel", [e]), loc), st)
              | TFfi ("Basis", "time") => ((EFfiApp ("Basis", "jsifyTime", [e]), loc), st)

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
                                              strcat loc [str loc "{_1:",
                                                          e',
                                                          str loc ",_2:",
                                                          (EApp ((ENamed n', loc),
                                                                 (EField ((ERel 0, loc), "2"), loc)), loc),
                                                          str loc "}"])],
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
                TRecord [] => ("(i++,null)", st)
              | TFfi ("Basis", "unit") => ("(i++,null)", st)
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
              | TFfi ("Basis", "char") => ("uu(t[i++])", st)
              | TFfi ("Basis", "int") => ("parseInt(t[i++])", st)
              | TFfi ("Basis", "time") => ("parseInt(t[i++])", st)
              | TFfi ("Basis", "float") => ("parseFloat(t[i++])", st)
              | TFfi ("Basis", "channel") => ("(t[i++].length > 0 ? parseInt(t[i-1]) : null)", st)

              | TFfi ("Basis", "bool") => ("t[i++] == \"1\"", st)

              | TSource => ("parseSource(t[i++], t[i++])", st)

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

                        fun patCon pc =
                            case pc of
                                PConVar n => str (Int.toString n)
                              | PConFfi {mod = "Basis", con = "True", ...} => str "true"
                              | PConFfi {mod = "Basis", con = "False", ...} => str "false"
                              | PConFfi {con, ...} => str ("\"" ^ con ^ "\"")

                        fun unsupported s =
                            (EM.errorAt loc (s ^ " in code to be compiled to JavaScript[2]");
                             Print.preface ("Code", MonoPrint.p_exp MonoEnv.empty e);
                             (str "ERROR", st))

                        val strcat = strcat loc

                        fun jsPrim p =
                            let
                                fun jsChar ch =
                                    case ch of
                                        #"'" =>
                                        if mode = Attribute then
                                            "\\047"
                                        else
                                            "'"
                                      | #"\"" => "\\\""
                                      | #"<" => "\\074"
                                      | #"\\" => "\\\\"
                                      | #"\n" => "\\n"
                                      | #"\r" => "\\r"
                                      | #"\t" => "\\t"
                                      | ch =>
                                        if Char.isPrint ch orelse ord ch >= 128 then
                                            String.str ch
                                        else
                                            "\\" ^ padWith (#"0",
                                                            Int.fmt StringCvt.OCT (ord ch),
                                                            3)
                            in
                                case p of
                                    Prim.String s =>
                                    str ("\"" ^ String.translate jsChar s ^ "\"")
                                  | Prim.Char ch => str ("\"" ^ jsChar ch ^ "\"")
                                  | _ => str (Prim.toString p)
                            end

                        fun jsPat (p, _) =
                            case p of
                                PWild => str "{c:\"w\"}"
                              | PVar _ => str "{c:\"v\"}"
                              | PPrim p => strcat [str "{c:\"c\",v:",
                                                   jsPrim p,
                                                   str "}"]
                              | PCon (_, PConFfi {mod = "Basis", con = "True", ...}, NONE) =>
                                str "{c:\"c\",v:true}"
                              | PCon (_, PConFfi {mod = "Basis", con = "False", ...}, NONE) =>
                                str "{c:\"c\",v:false}"
                              | PCon (Option, _, NONE) =>
                                str "{c:\"c\",v:null}"
                              | PCon (Option, PConVar n, SOME p) =>
                                (case IM.find (someTs, n) of
                                     NONE => raise Fail "Jscomp: Not in someTs"
                                   | SOME t =>
                                     strcat [str ("{c:\"s\",n:"
                                                  ^ (if isNullable t then
                                                         "true"
                                                     else
                                                         "false")
                                                  ^ ",p:"),
                                             jsPat p,
                                             str "}"])
                              | PCon (_, pc, NONE) => strcat [str "{c:\"c\",v:",
                                                              patCon pc,
                                                              str "}"]
                              | PCon (_, pc, SOME p) => strcat [str "{c:\"1\",n:",
                                                                patCon pc,
                                                                str ",p:",
                                                                jsPat p,
                                                                str "}"]
                              | PRecord xps => strcat [str "{c:\"r\",l:",
                                                       foldr (fn ((x, p, _), e) =>
                                                                 strcat [str ("cons({n:\"" ^ x ^ "\",p:"),
                                                                         jsPat p,
                                                                         str "},",
                                                                         e,
                                                                         str ")"])
                                                             (str "null") xps,
                                                       str "}"]
                              | PNone _ => str "{c:\"c\",v:null}"
                              | PSome (t, p) => strcat [str ("{c:\"s\",n:"
                                                             ^ (if isNullable t then
                                                                    "true"
                                                                else
                                                                    "false")
                                                             ^ ",p:"),
                                                        jsPat p,
                                                        str "}"]

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
                        (*Print.prefaces "jsE" [("e", MonoPrint.p_exp MonoEnv.empty e),
                                              ("inner", Print.PD.string (Int.toString inner))];*)

                        case #1 e of
                            EPrim p => (strcat [str "{c:\"c\",v:",
                                                jsPrim p,
                                                str "}"],
                                        st)
                          | ERel n =>
                            if n < inner then
                                (str ("{c:\"v\",n:" ^ Int.toString n ^ "}"), st)
                            else
                                let
                                    val n = n - inner
                                    (*val () = Print.prefaces "quote" [("t", MonoPrint.p_typ MonoEnv.empty
                                                                           (List.nth (outer, n)))]*)
                                    val (e, st) = quoteExp (List.nth (outer, n)) ((ERel n, loc), st)
                                in
                                    (strcat [str "{c:\"c\",v:",
                                             e,
                                             str "}"], st)
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
                                                val (e, st) = jsExp mode [] (e, st)
                                                val e = deStrcat 0 e
                                                val e = String.translate (fn #"'" => "\\'"
                                                                           | #"\\" => "\\\\"
                                                                           | ch => String.str ch) e
                                                
                                                val sc = "urfuncs[" ^ Int.toString n ^ "] = {c:\"t\",f:'"
                                                         ^ e ^ "'};\n"
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
                                (str ("{c:\"n\",n:" ^ Int.toString n ^ "}"), st)
                            end

                          | ECon (Option, _, NONE) => (str "{c:\"c\",v:null}", st)
                          | ECon (Option, PConVar n, SOME e) =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                case IM.find (someTs, n) of
                                    NONE => raise Fail "Jscomp: Not in someTs [2]"
                                  | SOME t =>
                                    (if isNullable t then
                                         strcat [str "{c:\"s\",v:",
                                                 e,
                                                 str "}"]
                                     else
                                         e, st)
                            end

                          | ECon (_, pc, NONE) => (strcat [str "{c:\"c\",v:",
                                                           patCon pc,
                                                           str "}"],
                                                   st)
                          | ECon (_, pc, SOME e) =>
                            let
                                val (s, st) = jsE inner (e, st)
                            in
                                (strcat [str "{c:\"1\",n:",
                                         patCon pc,
                                         str ",v:",
                                         s,
                                         str "}"], st)
                            end

                          | ENone _ => (str "{c:\"c\",v:null}", st)
                          | ESome (t, e) =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (if isNullable t then
                                     strcat [str "{c:\"s\",v:", e, str "}"]
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
                                (str ("{c:\"c\",v:" ^ name ^ "}"), st)
                            end
                          | EFfiApp ("Basis", "sigString", [_]) => (strcat [str "{c:\"c\",v:\"",
                                                                            e,
                                                                            str "\"}"], st)
                          | EFfiApp (m, x, args) =>
                            let
                                val name = case Settings.jsFunc (m, x) of
                                               NONE => (EM.errorAt loc ("Unsupported FFI function "
                                                                        ^ m ^ "." ^ x ^ " in JavaScript");
                                                        "ERROR")
                                             | SOME s => s

                                val (e, st) = foldr (fn (e, (acc, st)) =>
                                                        let
                                                            val (e, st) = jsE inner (e, st)
                                                        in
                                                            (strcat [str "cons(",
                                                                     e,
                                                                     str ",",
                                                                     acc,
                                                                     str ")"],
                                                             st)
                                                        end)
                                              (str "null", st) args
                            in
                                (strcat [str ("{c:\"f\",f:\"" ^ name ^ "\",a:"),
                                         e,
                                         str "}"],
                                 st)
                            end

                          | EApp (e1, e2) =>
                            let
                                val (e1, st) = jsE inner (e1, st)
                                val (e2, st) = jsE inner (e2, st)
                            in
                                (strcat [str "{c:\"a\",f:",
                                         e1,
                                         str ",x:",
                                         e2,
                                         str "}"], st)
                            end
                          | EAbs (_, _, _, e) =>
                            let
                                val (e, st) = jsE (inner + 1) (e, st)
                            in
                                (strcat [str "{c:\"l\",b:",
                                         e,
                                         str "}"], st)
                            end

                          | EUnop (s, e) =>
                            let
                                val name = case s of
                                               "!" => "not"
                                             | "-" => "neg"
                                             | _ => raise Fail ("Jscomp: Unknown unary operator " ^ s)

                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str ("{c:\"f\",f:\"" ^ name ^ "\",a:cons("),
                                         e,
                                         str ",null)}"],
                                 st)
                            end
                          | EBinop (bi, s, e1, e2) =>
                            let
                                val name = case s of
                                               "==" => "eq"
                                             | "!strcmp" => "eq"
                                             | "+" => "plus"
                                             | "-" => "minus"
                                             | "*" => "times"
                                             | "/" => (case bi of Int => "divInt" | NotInt => "div")
                                             | "%" => (case bi of Int => "modInt" | NotInt => "mod")
                                             | "fdiv" => "div"
                                             | "fmod" => "mod"
                                             | "<" => "lt"
                                             | "<=" => "le"
                                             | "strcmp" => "strcmp"
                                             | _ => raise Fail ("Jscomp: Unknown binary operator " ^ s)

                                val (e1, st) = jsE inner (e1, st)
                                val (e2, st) = jsE inner (e2, st)
                            in
                                (strcat [str ("{c:\"f\",f:\"" ^ name ^ "\",a:cons("),
                                         e1,
                                         str ",cons(",
                                         e2,
                                         str ",null))}"],
                                 st)
                            end

                          | ERecord [] => (str "{c:\"c\",v:null}", st)
                          | ERecord xes =>
                            let
                                val (es, st) =
                                    foldr (fn ((x, e, _), (es, st)) =>
                                              let
                                                  val (e, st) = jsE inner (e, st)
                                              in
                                                  (strcat [str ("cons({n:\"" ^ x ^ "\",v:"),
                                                           e,
                                                           str "},",
                                                           es,
                                                           str ")"],
                                                   st)
                                              end)
                                          (str "null", st) xes
                            in
                                (strcat [str "{c:\"r\",l:",
                                         es,
                                         str "}"],
                                 st)
                            end
                          | EField (e', x) =>
                            let
                                fun default () =
                                    let
                                        val (e', st) = jsE inner (e', st)
                                    in
                                        (strcat [str "{c:\".\",r:",
                                                 e',
                                                 str (",f:\"" ^ x ^ "\"}")], st)
                                    end

                                fun seek (e, xs) =
                                    case #1 e of
                                        ERel n =>
                                        if n < inner then
                                            default ()
                                        else
                                            let
                                                val n = n - inner
                                                val t = List.nth (outer, n)
                                                val t = foldl (fn (x, (TRecord xts, _)) =>
                                                                  (case List.find (fn (x', _) => x' = x) xts of
                                                                       NONE => raise Fail "Jscomp: Bad seek [1]"
                                                                     | SOME (_, t) => t)
                                                                | _ => raise Fail "Jscomp: Bad seek [2]")
                                                              t xs

                                                val e = (ERel n, loc)
                                                val e = foldl (fn (x, e) => (EField (e, x), loc)) e xs
                                                val (e, st) = quoteExp t (e, st)
                                            in
                                                (strcat [str "{c:\"c\",v:",
                                                         e,
                                                         str "}"],
                                                 st)
                                            end
                                      | EField (e', x) => seek (e', x :: xs)
                                      | _ => default ()
                            in
                                seek (e', [x])
                            end  

                          | ECase (e', pes, _) =>
                            let
                                val (e', st) = jsE inner (e', st)

                                val (ps, st) =
                                    foldr (fn ((p, e), (ps, st)) =>
                                              let
                                                  val (e, st) = jsE (inner + E.patBindsN p) (e, st)
                                              in
                                                  (strcat [str "cons({p:",
                                                           jsPat p,
                                                           str ",b:",
                                                           e,
                                                           str "},",
                                                           ps,
                                                           str ")"],
                                                   st)
                                              end)
                                          (str "null", st) pes
                            in
                                (strcat [str "{c:\"m\",e:",
                                         e',
                                         str ",p:",
                                         ps,
                                         str "}"], st)
                            end

                          | EStrcat (e1, e2) =>
                            let
                                val (e1, st) = jsE inner (e1, st)
                                val (e2, st) = jsE inner (e2, st)
                            in
                                (strcat [str "{c:\"f\",f:\"cat\",a:cons(", e1, str ",cons(", e2, str ",null))}"], st)
                            end

                          | EError (e, _) =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str "{c:\"f\",f:\"er\",a:cons(", e, str ",null)}"],
                                 st)
                            end

                          | ESeq (e1, e2) =>
                            let
                                val (e1, st) = jsE inner (e1, st)
                                val (e2, st) = jsE inner (e2, st)
                            in
                                (strcat [str "{c:\";\",e1:", e1, str ",e2:", e2, str "}"], st)
                            end
                          | ELet (_, _, e1, e2) =>
                            let
                                val (e1, st) = jsE inner (e1, st)
                                val (e2, st) = jsE (inner + 1) (e2, st)
                            in
                                (strcat [str "{c:\"=\",e1:",
                                         e1,
                                         str ",e2:",
                                         e2,
                                         str "}"], st)
                            end

                          | EJavaScript (Source _, e) =>
                            (foundJavaScript := true;
                             jsE inner (e, st))
                          | EJavaScript (_, e) =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                foundJavaScript := true;
                                (strcat [str "{c:\"e\",e:",
                                         e,
                                         str "}"],
                                 st)
                            end

                          | EWrite _ => unsupported "EWrite"
                          | EClosure _ => unsupported "EClosure"
                          | EQuery _ => unsupported "Query"
                          | EDml _ => unsupported "DML"
                          | ENextval _ => unsupported "Nextval"
                          | ESetval _ => unsupported "Nextval"
                          | EReturnBlob _ => unsupported "EReturnBlob"

                          | ERedirect (e, _) =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str "{c:\"f\",f:\"redirect\",a:cons(",
                                         e,
                                         str ",null)}"],
                                 st)
                            end

                          | EUnurlify (_, _, true) => unsupported "EUnurlify"

                          | EUnurlify (e, t, false) =>
                            let
                                val (e, st) = jsE inner (e, st)
                                val (e', st) = unurlifyExp loc (t, st)
                            in
                                (strcat [str ("{c:\"f\",f:\"unurlify\",a:cons({c:\"c\",v:function(s){var t=s.split(\"/\");var i=0;return "
                                              ^ e' ^ "}},cons("),
                                         e,
                                         str ",null))}"],
                                 st)
                            end

                          | ESignalReturn e =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str "{c:\"f\",f:\"sr\",a:cons(",
                                         e,
                                         str ",null)}"],
                                 st)
                            end
                          | ESignalBind (e1, e2) =>
                            let
                                val (e1, st) = jsE inner (e1, st)
                                val (e2, st) = jsE inner (e2, st)
                            in
                                (strcat [str "{c:\"f\",f:\"sb\",a:cons(",
                                         e1,
                                         str ",cons(",
                                         e2,
                                         str ",null))}"],
                                 st)
                            end
                          | ESignalSource e =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str "{c:\"f\",f:\"ss\",a:cons(",
                                         e,
                                         str ",null)}"],
                                 st)
                            end

                          | EServerCall (e, t, eff) =>
                            let
                                val (e, st) = jsE inner (e, st)
                                val (unurl, st) = unurlifyExp loc (t, st)
                            in
                                (strcat [str ("{c:\"f\",f:\"rc\",a:cons({c:\"c\",v:\""
                                              ^ Settings.getUrlPrefix ()
                                              ^ "\"},cons("),
                                         e,
                                         str (",cons({c:\"c\",v:function(s){var t=s.split(\"/\");var i=0;return "
                                              ^ unurl ^ "}},cons({c:\"K\"},cons({c:\"c\",v:"
                                              ^ (case eff of
                                                     ReadCookieWrite => "true"
                                                   | _ => "false")
                                              ^ "},null)))))}")],
                                 st)
                            end

                          | ERecv (e, t) =>
                            let
                                val (e, st) = jsE inner (e, st)
                                val (unurl, st) = unurlifyExp loc (t, st)
                            in
                                (strcat [str ("{c:\"f\",f:\"rv\",a:cons("),
                                         e,
                                         str (",cons({c:\"c\",v:function(s){var t=s.split(\"/\");var i=0;return "
                                              ^ unurl ^ "}},cons({c:\"K\"},null)))}")],
                                 st)
                            end

                          | ESleep e =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str "{c:\"f\",f:\"sl\",a:cons(",
                                         e,
                                         str ",cons({c:\"K\"},null))}"],
                                 st)
                            end

                          | ESpawn e =>
                            let
                                val (e, st) = jsE inner (e, st)
                            in
                                (strcat [str "{c:\"f\",f:\"sp\",a:cons(",
                                         e,
                                         str ",null)}"],
                                 st)
                            end
                    end
            in
                jsE 0
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
               | EBinop (bi, s, e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((EBinop (bi, s, e1, e2), loc), st)
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
               | ERedirect (e, t) =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((ERedirect (e, t), loc), st)
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
                     val row = exps @ map (fn (x, xts) => (x, (TRecord xts, loc))) tables
                     val row = ListMergeSort.sort (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) row
                     val row = (TRecord row, loc)

                     val (query, st) = exp outer (query, st)
                     val (body, st) = exp (state :: row :: outer) (body, st)
                     val (initial, st) = exp outer (initial, st)
                 in
                     ((EQuery {exps = exps, tables = tables, state = state,
                               query = query, body = body, initial = initial}, loc), st)
                 end
               | EDml (e, mode) =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((EDml (e, mode), loc), st)
                 end
               | ENextval e =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((ENextval e, loc), st)
                 end
               | ESetval (e1, e2) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                     val (e2, st) = exp outer (e2, st)
                 in
                     ((ESetval (e1, e2), loc), st)
                 end

               | EUnurlify (e, t, b) =>
                 let
                     val (e, st) = exp outer (e, st)
                 in
                     ((EUnurlify (e, t, b), loc), st)
                 end

               | EJavaScript (m as Source t, e') =>
                 (foundJavaScript := true;
                  let
                      val (x', st) = jsExp m (t :: outer) ((ERel 0, loc), st)
                  in
                      ((ELet ("x", t, e', x'), loc), st)
                  end
                  handle CantEmbed _ =>
                         (jsExp m outer (e', st)
                          handle CantEmbed t => ((*ErrorMsg.errorAt loc "Unable to embed type in JavaScript";
                                                  Print.preface ("Type",
                                                                 MonoPrint.p_typ MonoEnv.empty t);*)
                                                 (e, st))))

               | EJavaScript (m, e') =>
                 (foundJavaScript := true;
                  jsExp m outer (e', st)
                  handle CantEmbed t => ((*ErrorMsg.errorAt loc "Unable to embed type in JavaScript";
                                         Print.preface ("Type",
                                                        MonoPrint.p_typ MonoEnv.empty t);*)
                                         (e, st)))

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
                 
               | EServerCall (e1, t, ef) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                 in
                     ((EServerCall (e1, t, ef), loc), st)
                 end
               | ERecv (e1, t) =>
                 let
                     val (e1, st) = exp outer (e1, st)
                 in
                     ((ERecv (e1, t), loc), st)
                 end
               | ESleep e1 =>
                 let
                     val (e1, st) = exp outer (e1, st)
                 in
                     ((ESleep e1, loc), st)
                 end
               | ESpawn e1 =>
                 let
                     val (e1, st) = exp outer (e1, st)
                 in
                     ((ESpawn e1, loc), st)
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

        val urlRules = foldr (fn (r, s) =>
                                 "cons({allow:"
                                 ^ (if #action r = Settings.Allow then "true" else "false")
                                 ^ ",prefix:"
                                 ^ (if #kind r = Settings.Prefix then "true" else "false")
                                 ^ ",pattern:\""
                                 ^ #pattern r
                                 ^ "\"},"
                                 ^ s
                                 ^ ")") "null" (Settings.getUrlRules ())

        val urlRules = "urlRules = " ^ urlRules ^ ";\n\n"

        val script =
            if !foundJavaScript then
                lines ^ urlRules ^ String.concat (rev (#script st))
                ^ "\ntime_format = \"" ^ String.toCString (Settings.getTimeFormat ()) ^ "\";\n"
            else
                ""
    in
        TextIO.closeIn inf;
        (DJavaScript script, ErrorMsg.dummySpan) :: ds
    end

end
