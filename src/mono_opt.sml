(* Copyright (c) 2008-2010, Adam Chlipala
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

structure MonoOpt :> MONO_OPT = struct

open Mono
structure U = MonoUtil

fun typ t = t
fun decl d = d

fun attrifyInt n =
    if n < 0 then
        "-" ^ Int64.toString (Int64.~ n)
    else
        Int64.toString n

fun attrifyFloat n =
    if n < 0.0 then
        "-" ^ Real.toString (Real.~ n)
    else
        Real.toString n

fun attrifyChar ch =
    case ch of
        #"\"" => "&quot;"
      | #"&" => "&amp;"
      | ch => str ch

val attrifyString = String.translate attrifyChar


val urlifyInt = attrifyInt
val urlifyFloat = attrifyFloat

val htmlifyInt = attrifyInt
val htmlifyFloat = attrifyFloat

val htmlifyString = String.translate (fn #"<" => "&lt;"
                                       | #"&" => "&amp;"
                                       | ch => str ch)

fun htmlifySpecialChar ch = "&#" ^ Int.toString (ord ch) ^ ";"

fun hexPad c =
    let
        val s = Int.fmt StringCvt.HEX c
    in
        case size s of
            0 => "00"
          | 1 => "0" ^ s
          | _ => s
    end

fun rsh a b =
    Int.fromLarge (IntInf.~>>(IntInf.fromInt a, Word.fromInt b))

fun orb a b =
    Int.fromLarge (IntInf.orb(IntInf.fromInt a, IntInf.fromInt b))

fun andb a b =
    Int.fromLarge (IntInf.andb(IntInf.fromInt a, IntInf.fromInt b))


fun hexIt ch =
    let
        val c = ord ch
    in
        if (c <= 0x7f) then
            hexPad c
        else
            ((if (c <= 0x7fff) then
                 hexPad (orb (rsh c 6) 0xc0) 
             else
                 (if (c <= 0xffff) then
                      hexPad (orb (rsh c 12) 0xe0)
                  else
                      hexPad (orb (rsh c 18) 0xf0)
                      ^ hexPad (orb (andb (rsh c 12) 0x3f) 0x80)
                 )
                 ^ hexPad (orb (andb (rsh c 6) 0x3f) 0x80))
             ) ^ hexPad (orb (andb c 0x3f) 0x80)
    end

fun urlifyCharAux ch =
    case ch of
        #" "  => "+"
     |  _ =>
        if ord ch = 0 then
            "_"
        else
            if Char.isAlphaNum ch then
                str ch
            else
                "." ^ hexIt ch
        
fun urlifyChar c =
    case c of
        #"_" => "_" ^ urlifyCharAux c 
     |  _ => urlifyCharAux c
                  
        
fun urlifyString s =
    case s of
        "" => "_"
      | _ =>
        (if String.sub (s, 0) = #"_" then
             "_"
         else
             "")
        ^ String.translate urlifyCharAux s


fun sqlifyInt n = #p_cast (Settings.currentDbms ()) (attrifyInt n, Settings.Int)
fun sqlifyFloat n = #p_cast (Settings.currentDbms ()) (attrifyFloat n, Settings.Float)

fun sqlifyString s = #sqlifyString (Settings.currentDbms ()) s
fun sqlifyChar ch = #sqlifyString (Settings.currentDbms ()) (str ch)

fun unAs s =
    let
        fun doChars (cs, acc) =
            case cs of
                #"T" :: #"_" :: #"T" :: #"." :: cs => doChars (cs, acc)
              | #"'" :: cs => doString (cs, #"'" :: acc)
              | ch :: cs => doChars (cs, ch :: acc)
              | [] => String.implode (rev acc)

        and doString (cs, acc) =
            case cs of
                #"\\" :: #"\\" :: cs => doString (cs, #"\\" :: #"\\" :: acc)
              | #"\\" :: #"'" :: cs => doString (cs, #"'" :: #"\\" :: acc)
              | #"'" :: cs => doChars (cs, #"'" :: acc)
              | ch :: cs => doString (cs, ch :: acc)
              | [] => String.implode (rev acc)
    in
        doChars (String.explode s, [])
    end

fun checkUrl s = CharVector.all Char.isGraph s andalso Settings.checkUrl s
val checkData = CharVector.all (fn ch => Char.isAlphaNum ch
                                         orelse ch = #"_"
                                         orelse ch = #"-")
val checkAtom = CharVector.all (fn ch => Char.isAlphaNum ch
                                         orelse ch = #"+"
                                         orelse ch = #"-"
                                         orelse ch = #"."
                                         orelse ch = #"%"
                                         orelse ch = #"#")
val checkCssUrl = CharVector.all (fn ch => Char.isAlphaNum ch
                                           orelse ch = #":"
                                           orelse ch = #"/"
                                           orelse ch = #"."
                                           orelse ch = #"_"
                                           orelse ch = #"+"
                                           orelse ch = #"-"
                                           orelse ch = #"%"
                                           orelse ch = #"?"
                                           orelse ch = #"&"
                                           orelse ch = #"="
                                           orelse ch = #"#")
fun checkProperty s =
  (* See https://www.w3.org/TR/CSS21/grammar.html#scanner, rule `ident` *)
  let
    fun nmstart ch = Char.isAlpha ch orelse ch = #"_"
    fun nmchar ch = nmstart ch orelse Char.isDigit ch orelse ch = #"-"
  in
    size s > 0
    andalso (nmstart (String.sub (s, 0)) orelse size s > 1 andalso String.sub (s, 0) = #"-" andalso nmstart (String.sub (s, 1)))
    andalso CharVector.all nmchar s
  end

fun exp e =
    case e of
        EPrim (Prim.String (Prim.Html, s)) =>
        if CharVector.exists Char.isSpace s then
            let
                val (_, chs) =
                    CharVector.foldl (fn (ch, (lastSpace, chs)) =>
                                         let
                                             val isSpace = Char.isSpace ch
                                         in
                                             if isSpace andalso lastSpace then
                                                 (true, chs)
                                             else
                                                 (isSpace, ch :: chs)
                                         end)
                                     (false, []) s
            in
                EPrim (Prim.String (Prim.Html, String.implode (rev chs)))
            end
        else
            e

      | EFfiApp ("Basis", "strcat", [(e1, _), (e2, _)]) => exp (EStrcat (e1, e2))

      | EStrcat (e1, (EPrim (Prim.String (_, "")), _)) => #1 e1
      | EStrcat ((EPrim (Prim.String (_, "")), _), e2) => #1 e2

      | EStrcat ((EPrim (Prim.String (Prim.Html, s1)), loc), (EPrim (Prim.String (Prim.Html, s2)), _)) =>
        let
            val s =
                if size s1 > 0 andalso size s2 > 0
                   andalso Char.isSpace (String.sub (s1, size s1 - 1))
                   andalso Char.isSpace (String.sub (s2, 0)) then
                    s1 ^ String.extract (s2, 1, NONE)
                else
                    s1 ^ s2
        in
            EPrim (Prim.String (Prim.Html, s))
        end

      | EStrcat ((EPrim (Prim.String (_, s1)), loc), (EPrim (Prim.String (_, s2)), _)) =>
        EPrim (Prim.String (Prim.Normal, s1 ^ s2))

      | EStrcat ((EPrim (Prim.String (Prim.Html, s1)), loc), (EStrcat ((EPrim (Prim.String (Prim.Html, s2)), _), rest), _)) =>
        let
            val s =
                if size s1 > 0 andalso size s2 > 0
                   andalso Char.isSpace (String.sub (s1, size s1 - 1))
                   andalso Char.isSpace (String.sub (s2, 0)) then
                    s1 ^ String.extract (s2, 1, NONE)
                else
                    s1 ^ s2
        in
            EStrcat ((EPrim (Prim.String (Prim.Html, s)), loc), rest)
        end

      | EStrcat ((EPrim (Prim.String (_, s1)), loc), (EStrcat ((EPrim (Prim.String (_, s2)), _), rest), _)) =>
        EStrcat ((EPrim (Prim.String (Prim.Normal, s1 ^ s2)), loc), rest)

      | EStrcat ((EStrcat (e1, e2), loc), e3) =>
        optExp (EStrcat (e1, (EStrcat (e2, e3), loc)), loc)

      | EWrite (EStrcat (e1, e2), loc) =>
        ESeq ((optExp (EWrite e1, loc), loc),
              (optExp (EWrite e2, loc), loc))

      | ESeq ((EWrite (EPrim (Prim.String (_, s1)), _), loc),
              (EWrite (EPrim (Prim.String (_, s2)), _), _)) =>
        EWrite (EPrim (Prim.String (Prim.Normal, s1 ^ s2)), loc)
      | ESeq ((EWrite (EPrim (Prim.String (_, s1)), _), loc),
              (ESeq ((EWrite (EPrim (Prim.String (_, s2)), _), _),
                     e), _)) =>
        ESeq ((EWrite (EPrim (Prim.String (Prim.Normal, s1 ^ s2)), loc), loc),
              e)

      | EFfiApp ("Basis", "htmlifySpecialChar", [((EPrim (Prim.Char ch), _), _)]) =>
        EPrim (Prim.String (Prim.Html, htmlifySpecialChar ch))
      | EWrite (EFfiApp ("Basis", "htmlifySpecialChar", [e]), _) =>
        EFfiApp ("Basis", "htmlifySpecialChar_w", [e])

      | EWrite (EFfiApp ("Basis", "intToString", [e]), _) =>
        EFfiApp ("Basis", "htmlifyInt_w", [e])
      | EApp ((EFfi ("Basis", "intToString"), loc), e) =>
        EFfiApp ("Basis", "intToString", [(e, (TFfi ("Basis", "int"), loc))])

      | EFfiApp ("Basis", "htmlifyString", [((EFfiApp ("Basis", "intToString", [((EPrim (Prim.Int n), _), _)]), _), _)]) =>
        EPrim (Prim.String (Prim.Html, htmlifyInt n))
      | EFfiApp ("Basis", "htmlifyString", [((EFfiApp ("Basis", "intToString", es), _), _)]) =>
        EFfiApp ("Basis", "htmlifyInt", es)
      | EFfiApp ("Basis", "htmlifyString", [((EApp ((EFfi ("Basis", "intToString"), _),
                                                    (EPrim (Prim.Int n), _)), _), _)]) =>
        EPrim (Prim.String (Prim.Html, htmlifyInt n))
      | EFfiApp ("Basis", "htmlifyString", [((EApp ((EFfi ("Basis", "intToString"), _),
                                                    e), loc), _)]) =>
        EFfiApp ("Basis", "htmlifyInt", [(e, (TFfi ("Basis", "int"), loc))])
      | EWrite (EFfiApp ("Basis", "htmlifyInt", [e]), _) =>
        EFfiApp ("Basis", "htmlifyInt_w", [e])

      | EFfiApp ("Basis", "htmlifyString", [((EFfiApp ("Basis", "floatToString", [((EPrim (Prim.Float n), _), _)]), _), _)]) =>
        EPrim (Prim.String (Prim.Html, htmlifyFloat n))
      | EFfiApp ("Basis", "htmlifyString", [((EFfiApp ("Basis", "floatToString", es), _), _)]) =>
        EFfiApp ("Basis", "htmlifyFloat", es)
      | EFfiApp ("Basis", "htmlifyString", [((EApp ((EFfi ("Basis", "floatToString"), _),
                                                    (EPrim (Prim.Float n), _)), _), _)]) =>
        EPrim (Prim.String (Prim.Html, htmlifyFloat n))
      | EFfiApp ("Basis", "htmlifyString", [((EApp ((EFfi ("Basis", "floatToString"), _),
                                                    e), loc), _)]) =>
        EFfiApp ("Basis", "htmlifyFloat", [(e, (TFfi ("Basis", "float"), loc))])
      | EWrite (EFfiApp ("Basis", "htmlifyFloat", [e]), _) =>
        EFfiApp ("Basis", "htmlifyFloat_w", [e])

      | EFfiApp ("Basis", "htmlifyString", [((EFfiApp ("Basis", "boolToString",
                                                       [((ECon (Enum, PConFfi {con = "True", ...}, NONE), _), _)]), _), _)]) =>
        EPrim (Prim.String (Prim.Html, "True"))
      | EFfiApp ("Basis", "htmlifyString", [((EFfiApp ("Basis", "boolToString",
                                                       [((ECon (Enum, PConFfi {con = "False", ...}, NONE), _), _)]), _), _)]) =>
        EPrim (Prim.String (Prim.Html, "False"))
      | EFfiApp ("Basis", "htmlifyString", [((EFfiApp ("Basis", "boolToString", es), _), _)]) =>
        EFfiApp ("Basis", "htmlifyBool", es)
      | EFfiApp ("Basis", "htmlifyString", [((EApp ((EFfi ("Basis", "boolToString"), _),
                                                    (ECon (Enum, PConFfi {con = "True", ...}, NONE), _)), _), _)]) =>
        EPrim (Prim.String (Prim.Html, "True"))
      | EFfiApp ("Basis", "htmlifyString", [((EApp ((EFfi ("Basis", "boolToString"), _),
                                                    (ECon (Enum, PConFfi {con = "False", ...}, NONE), _)), _), _)]) =>
        EPrim (Prim.String (Prim.Html, "False"))
      | EFfiApp ("Basis", "htmlifyString", [((EApp ((EFfi ("Basis", "boolToString"), _),
                                                    e), loc), _)]) =>
        EFfiApp ("Basis", "htmlifyBool", [(e, (TFfi ("Basis", "bool"), loc))])
      | EWrite (EFfiApp ("Basis", "htmlifyBool", [e]), _) =>
        EFfiApp ("Basis", "htmlifyBool_w", [e])

      | EFfiApp ("Basis", "htmlifyString", [((EApp ((EFfi ("Basis", "timeToString"), _), e), loc), _)]) =>
        EFfiApp ("Basis", "htmlifyTime", [(e, (TFfi ("Basis", "time"), loc))])
      | EFfiApp ("Basis", "htmlifyString_w", [((EApp ((EFfi ("Basis", "timeToString"), loc), e), _), _)]) =>
        EFfiApp ("Basis", "htmlifyTime_w", [(e, (TFfi ("Basis", "time"), loc))])
      | EWrite (EFfiApp ("Basis", "htmlifyTime", [e]), _) =>
        EFfiApp ("Basis", "htmlifyTime_w", [e])

      | EFfiApp ("Basis", "htmlifyString", [((EPrim (Prim.String (_, s)), _), _)]) =>
        EPrim (Prim.String (Prim.Html, htmlifyString s))
      | EWrite (EFfiApp ("Basis", "htmlifyString", [((EPrim (Prim.String (_, s)), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Html, htmlifyString s)), loc)
      | EWrite (EFfiApp ("Basis", "htmlifyString", [e]), _) =>
        EFfiApp ("Basis", "htmlifyString_w", [e])
      | EFfiApp ("Basis", "htmlifyString_w", [((EPrim (Prim.String (_, s)), loc), _)]) =>
        EWrite (EPrim (Prim.String (Prim.Html, htmlifyString s)), loc)

      | EWrite (EFfiApp ("Basis", "htmlifySource", [e]), _) =>
        EFfiApp ("Basis", "htmlifySource_w", [e])

      | EFfiApp ("Basis", "attrifyInt", [((EPrim (Prim.Int n), _), _)]) =>
        EPrim (Prim.String (Prim.Html, attrifyInt n))
      | EWrite (EFfiApp ("Basis", "attrifyInt", [((EPrim (Prim.Int n), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Html, attrifyInt n)), loc)
      | EWrite (EFfiApp ("Basis", "attrifyInt", [e]), _) =>
        EFfiApp ("Basis", "attrifyInt_w", [e])

      | EFfiApp ("Basis", "attrifyFloat", [((EPrim (Prim.Float n), _), _)]) =>
        EPrim (Prim.String (Prim.Html, attrifyFloat n))
      | EWrite (EFfiApp ("Basis", "attrifyFloat", [((EPrim (Prim.Float n), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Html, attrifyFloat n)), loc)
      | EWrite (EFfiApp ("Basis", "attrifyFloat", [e]), _) =>
        EFfiApp ("Basis", "attrifyFloat_w", [e])

      | EFfiApp ("Basis", "attrifyString", [((EPrim (Prim.String (_, s)), _), _)]) =>
        EPrim (Prim.String (Prim.Html, attrifyString s))
      | EWrite (EFfiApp ("Basis", "attrifyString", [((EPrim (Prim.String (_, s)), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Html, attrifyString s)), loc)
      | EWrite (EFfiApp ("Basis", "attrifyString", [e]), _) =>
        EFfiApp ("Basis", "attrifyString_w", [e])

      | EFfiApp ("Basis", "attrifyChar", [((EPrim (Prim.Char s), _), _)]) =>
        EPrim (Prim.String (Prim.Html, attrifyChar s))
      | EWrite (EFfiApp ("Basis", "attrifyChar", [((EPrim (Prim.Char s), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Html, attrifyChar s)), loc)
      | EWrite (EFfiApp ("Basis", "attrifyChar", [e]), _) =>
        EFfiApp ("Basis", "attrifyChar_w", [e])

      | EFfiApp ("Basis", "attrifyCss_class", [((EPrim (Prim.String (_, s)), _), _)]) =>
        EPrim (Prim.String (Prim.Html, s))
      | EWrite (EFfiApp ("Basis", "attrifyCss_class", [((EPrim (Prim.String (_, s)), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Html, s)), loc)
      | EWrite (EFfiApp ("Basis", "attrifyCss_class", [e]), _) =>
        EFfiApp ("Basis", "attrifyString_w", [e])

      | EFfiApp ("Basis", "urlifyInt", [((EPrim (Prim.Int n), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, urlifyInt n))
      | EWrite (EFfiApp ("Basis", "urlifyInt", [((EPrim (Prim.Int n), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Normal, urlifyInt n)), loc)
      | EWrite (EFfiApp ("Basis", "urlifyInt", [e]), _) =>
        EFfiApp ("Basis", "urlifyInt_w", [e])

      | EFfiApp ("Basis", "urlifyFloat", [((EPrim (Prim.Float n), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, urlifyFloat n))
      | EWrite (EFfiApp ("Basis", "urlifyFloat", [((EPrim (Prim.Float n), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Normal, urlifyFloat n)), loc)
      | EWrite (EFfiApp ("Basis", "urlifyFloat", [e]), _) =>
        EFfiApp ("Basis", "urlifyFloat_w", [e])

      | EFfiApp ("Basis", "urlifyString", [((EPrim (Prim.String (_, s)), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, urlifyString s))
      | EWrite (EFfiApp ("Basis", "urlifyString", [((EPrim (Prim.String (Prim.Normal, s)), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Normal, urlifyString s)), loc)
      | EWrite (EFfiApp ("Basis", "urlifyString", [e]), _) =>
        EFfiApp ("Basis", "urlifyString_w", [e])

      | EFfiApp ("Basis", "urlifyChar", [((EPrim (Prim.Char c), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, urlifyChar c))
      | EWrite (EFfiApp ("Basis", "urlifyChar", [((EPrim (Prim.Char c), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Normal, urlifyChar c)), loc)
      | EWrite (EFfiApp ("Basis", "urlifyChar", [e]), _) =>
        EFfiApp ("Basis", "urlifyChar_w", [e])
                
      | EFfiApp ("Basis", "urlifyBool", [((ECon (Enum, PConFfi {con = "True", ...}, NONE), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, "1"))
      | EFfiApp ("Basis", "urlifyBool", [((ECon (Enum, PConFfi {con = "False", ...}, NONE), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, "0"))
      | EWrite (EFfiApp ("Basis", "urlifyBool", [((ECon (Enum, PConFfi {con = "True", ...}, NONE), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Normal, "1")), loc)
      | EWrite (EFfiApp ("Basis", "urlifyBool", [((ECon (Enum, PConFfi {con = "False", ...}, NONE), _), _)]), loc) =>
        EWrite (EPrim (Prim.String (Prim.Normal, "0")), loc)
      | EWrite (EFfiApp ("Basis", "urlifyBool", [e]), _) =>
        EFfiApp ("Basis", "urlifyBool_w", [e])

      | EFfiApp ("Basis", "sqlifyInt", [((EPrim (Prim.Int n), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, sqlifyInt n))
      | EFfiApp ("Basis", "sqlifyIntN", [((ENone _, _), _)]) =>
        EPrim (Prim.String (Prim.Normal, "NULL"))
      | EFfiApp ("Basis", "sqlifyIntN", [((ESome (_, (EPrim (Prim.Int n), _)), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, sqlifyInt n))

      | EFfiApp ("Basis", "sqlifyFloat", [((EPrim (Prim.Float n), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, sqlifyFloat n))
      | EFfiApp ("Basis", "sqlifyBool", [(b as (_, loc), _)]) =>
        optExp (ECase (b,
                       [((PCon (Enum, PConFfi {mod = "Basis", datatyp = "bool", con = "True", arg = NONE}, NONE), loc),
                         (EPrim (Prim.String (Prim.Normal, #trueString (Settings.currentDbms ()))), loc)),
                        ((PCon (Enum, PConFfi {mod = "Basis", datatyp = "bool", con = "False", arg = NONE}, NONE), loc),
                         (EPrim (Prim.String (Prim.Normal, #falseString (Settings.currentDbms ()))), loc))],
                       {disc = (TFfi ("Basis", "bool"), loc),
                        result = (TFfi ("Basis", "string"), loc)}), loc)
      | EFfiApp ("Basis", "sqlifyString", [((EPrim (Prim.String (_, n)), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, sqlifyString n))
      | EFfiApp ("Basis", "sqlifyChar", [((EPrim (Prim.Char n), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, sqlifyChar n))

      | EWrite (ECase (discE, pes, {disc, ...}), loc) =>
        optExp (ECase (discE,
                       map (fn (p, e) => (p, (EWrite e, loc))) pes,
                       {disc = disc,
                        result = (TRecord [], loc)}), loc)

      | EApp ((ECase (discE, pes, {disc, result = (TFun (_, ran), _)}), loc), arg as (ERecord [], _)) =>
        let
            fun doBody e =
                case #1 e of
                    EAbs (_, _, _, body) => MonoReduce.subExpInExp (0, arg) body
                  | _ => (EApp (e, arg), loc)
        in
            optExp (ECase (discE,
                           map (fn (p, e) => (p, doBody e)) pes,
                           {disc = disc,
                            result = ran}), loc)
        end

      | EWrite (EQuery {exps, tables, state, query,
                        initial = (EPrim (Prim.String (k, "")), _),
                        body = (EStrcat ((EPrim (Prim.String (_, s)), _),
                                         (EStrcat ((ERel 0, _),
                                                   e'), _)), _)}, loc) =>
        if (case k of Prim.Normal => s = "" | Prim.Html => CharVector.all Char.isSpace s) then
            EQuery {exps = exps, tables = tables, query = query,
                    state = (TRecord [], loc),
                    initial = (ERecord [], loc),
                    body = (optExp (EWrite e', loc), loc)}
        else
            e

      | EWrite (EQuery {exps, tables, state, query,
                        initial = (EPrim (Prim.String (_, "")), _),
                        body}, loc) =>
        let
            fun passLets (depth, (e', _), lets) =
                case e' of
                    EStrcat ((ERel x, _), e'') =>
                    if x = depth then
                        let
                            val body = (optExp (EWrite e'', loc), loc)
                            val body = foldl (fn ((x, t, e'), e) =>
                                                 (ELet (x, t, e', e), loc))
                                             body lets
                        in
                            EQuery {exps = exps, tables = tables, query = query,
                                    state = (TRecord [], loc),
                                    initial = (ERecord [], loc),
                                    body = body}
                        end
                    else
                        e
                  | ELet (x, t, e', e'') =>
                    passLets (depth + 1, e'', (x, t, e') :: lets)
                  | _ => e
        in
            passLets (0, body, [])
        end

      (*| EWrite (EQuery {exps, tables, state, query,
                        initial = (EPrim (Prim.String ""), _),
                        body = (EStrcat ((ERel 0, _), e'), _)}, loc) =>
        EQuery {exps = exps, tables = tables, query = query,
                state = (TRecord [], loc),
                initial = (ERecord [], loc),
                body = (optExp (EWrite e', loc), loc)}*)

      | EWrite (ELet (x, t, e1, e2), loc) =>
        optExp (ELet (x, t, e1, (EWrite e2, loc)), loc)

      | EWrite (EPrim (Prim.String (_, "")), loc) =>
        ERecord []

      | ESignalBind ((ESignalReturn e1, loc), e2) =>
        optExp (EApp (e2, e1), loc)

      | EFfiApp ("Basis", "blessData", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if checkData s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid HTML5 data-* attribute " ^ s);
         se)

      | EFfiApp ("Basis", "bless", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if checkUrl s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid URL " ^ s ^ " passed to 'bless'");
         se)
      | EFfiApp ("Basis", "checkUrl", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if checkUrl s then
             ESome ((TFfi ("Basis", "string"), loc), (se, loc))
         else
             ENone (TFfi ("Basis", "string"), loc))
      | EFfiApp ("Basis", "blessMime", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkMime s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid string " ^ s ^ " passed to 'blessMime'");
         se)
      | EFfiApp ("Basis", "checkMime", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkMime s then
             ESome ((TFfi ("Basis", "string"), loc), (se, loc))
         else
             ENone (TFfi ("Basis", "string"), loc))
      | EFfiApp ("Basis", "atom", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if checkAtom s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid string " ^ s ^ " passed to 'atom'");
         se)
      | EFfiApp ("Basis", "css_url", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if checkCssUrl s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid URL " ^ s ^ " passed to 'css_url'");
         se)
      | EFfiApp ("Basis", "property", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if checkProperty s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid string " ^ s ^ " passed to 'property'");
         se)
      | EFfiApp ("Basis", "blessRequestHeader", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkRequestHeader s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid string " ^ s ^ " passed to 'blessRequestHeader'");
         se)
      | EFfiApp ("Basis", "checkRequestHeader", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkRequestHeader s then
             ESome ((TFfi ("Basis", "string"), loc), (se, loc))
         else
             ENone (TFfi ("Basis", "string"), loc))
      | EFfiApp ("Basis", "blessResponseHeader", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkResponseHeader s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid string " ^ s ^ " passed to 'blessResponseHeader'");
         se)
      | EFfiApp ("Basis", "checkResponseHeader", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkResponseHeader s then
             ESome ((TFfi ("Basis", "string"), loc), (se, loc))
         else
             ENone (TFfi ("Basis", "string"), loc))
      | EFfiApp ("Basis", "blessEnvVar", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkEnvVar s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid string " ^ s ^ " passed to 'blessEnvVar'");
         se)
      | EFfiApp ("Basis", "checkEnvVar", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkEnvVar s then
             ESome ((TFfi ("Basis", "string"), loc), (se, loc))
         else
             ENone (TFfi ("Basis", "string"), loc))
      | EFfiApp ("Basis", "blessMeta", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkMeta s then
             ()
         else
             ErrorMsg.errorAt loc ("Invalid string " ^ s ^ " passed to 'blessMeta'");
         se)
      | EFfiApp ("Basis", "checkMeta", [((se as EPrim (Prim.String (_, s)), loc), _)]) =>
        (if Settings.checkMeta s then
             ESome ((TFfi ("Basis", "string"), loc), (se, loc))
         else
             ENone (TFfi ("Basis", "string"), loc))

      | EFfiApp ("Basis", "checkString", [((EPrim (Prim.String (_, s)), loc), _)]) =>
        let
            fun uwify (cs, acc) =
                case cs of
                    [] => String.concat (rev acc)
                  | #"(" :: #"_" :: cs => uwify (cs, "(uw_" :: acc)
                  | #" " :: #"_" :: cs => uwify (cs, " uw_" :: acc)
                  | #"'" :: cs =>
                    let
                        fun waitItOut (cs, acc) =
                            case cs of
                                [] => raise Fail "MonoOpt: Unterminated SQL string literal"
                              | #"'" :: cs => uwify (cs, "'" :: acc)
                              | #"\\" :: #"'" :: cs => waitItOut (cs, "\\'" :: acc)
                              | #"\\" :: #"\\" :: cs => waitItOut (cs, "\\\\" :: acc)
                              | c :: cs => waitItOut (cs, str c :: acc)
                    in
                        waitItOut (cs, "'" :: acc)
                    end
                  | c :: cs => uwify (cs, str c :: acc)

            val s = case String.explode s of
                        #"_" :: cs => uwify (cs, ["uw_"])
                      | cs => uwify (cs, [])
        in
            EPrim (Prim.String (Prim.Normal, s))
        end

      | EFfiApp ("Basis", "viewify", [((EPrim (Prim.String (_, s)), loc), _)]) =>
        let
            fun uwify (cs, acc) =
                case cs of
                    [] => String.concat (rev acc)
                  | #"A" :: #"S" :: #" " :: #"_" :: cs => uwify (cs, "AS uw_" :: acc)
                  | #"'" :: cs =>
                    let
                        fun waitItOut (cs, acc) =
                            case cs of
                                [] => raise Fail "MonoOpt: Unterminated SQL string literal"
                              | #"'" :: cs => uwify (cs, "'" :: acc)
                              | #"\\" :: #"'" :: cs => waitItOut (cs, "\\'" :: acc)
                              | #"\\" :: #"\\" :: cs => waitItOut (cs, "\\\\" :: acc)
                              | c :: cs => waitItOut (cs, str c :: acc)
                    in
                        waitItOut (cs, "'" :: acc)
                    end
                  | c :: cs => uwify (cs, str c :: acc)

            val s = uwify (String.explode s, [])
        in
            EPrim (Prim.String (Prim.Normal, s))
        end

      | EFfiApp ("Basis", "unAs", [((EPrim (Prim.String (_, s)), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, unAs s))
      | EFfiApp ("Basis", "unAs", [(e', _)]) =>
        let
            fun parts (e as (_, loc)) =
                case #1 e of
                    EStrcat (s1, s2) =>
                    (case (parts s1, parts s2) of
                         (SOME p1, SOME p2) => SOME (p1 @ p2)
                       | _ => NONE)
                  | EPrim (Prim.String (_, s)) => SOME [(EPrim (Prim.String (Prim.Normal, unAs s)), loc)]
                  | EFfiApp ("Basis", f, [_]) =>
                    if String.isPrefix "sqlify" f then
                        SOME [e]
                    else
                        NONE
                  | _ => NONE
        in
            case parts e' of
                SOME [e] => #1 e
              | SOME es =>
                (case rev es of
                     (e as (_, loc)) :: es => #1 (foldl (fn (e, es) => (EStrcat (e, es), loc)) e es)
                   | [] => raise Fail "MonoOpt impossible nil")
              | NONE => e
        end

      | EFfiApp ("Basis", "str1", [((EPrim (Prim.Char ch), _), _)]) =>
        EPrim (Prim.String (Prim.Normal, str ch))
      | EFfiApp ("Basis", "attrifyString", [((EFfiApp ("Basis", "str1", [e]), _), _)]) =>
        EFfiApp ("Basis", "attrifyChar", [e])
      | EFfiApp ("Basis", "attrifyString_w", [((EFfiApp ("Basis", "str1", [e]), _), _)]) =>
        EFfiApp ("Basis", "attrifyChar_w", [e])
      | EWrite (EFfiApp ("Basis", "str1", [e]), _) =>
        EFfiApp ("Basis", "writec", [e])

      | EBinop (_, "+", (EPrim (Prim.Int n1), _), (EPrim (Prim.Int n2), _)) => EPrim (Prim.Int (Int64.+ (n1, n2)))
      | EBinop (_, "-", (EPrim (Prim.Int n1), _), (EPrim (Prim.Int n2), _)) => EPrim (Prim.Int (Int64.- (n1, n2)))
      | EBinop (_, "*", (EPrim (Prim.Int n1), _), (EPrim (Prim.Int n2), _)) => EPrim (Prim.Int (Int64.* (n1, n2)))

      | _ => e

and optExp e = #1 (U.Exp.map {typ = typ, exp = exp} e)

val optimize = U.File.map {typ = typ, exp = exp, decl = decl}

val optExp = U.Exp.map {typ = typ, exp = exp}

end
