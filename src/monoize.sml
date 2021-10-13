(* Copyright (c) 2008-2014, Adam Chlipala
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

structure Monoize :> MONOIZE = struct

structure E = ErrorMsg
structure Env = CoreEnv

structure L = Core
structure L' = Mono

structure IM = IntBinaryMap
structure IS = IntBinarySet

structure SK = struct
type ord_key = string
val compare = String.compare
end

structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)

structure RM = BinaryMapFn(struct
                           type ord_key = (string * L'.typ) list
                           fun compare (r1, r2) = MonoUtil.Typ.compare ((L'.TRecord r1, E.dummySpan),
                                                                        (L'.TRecord r2, E.dummySpan))
                           end)

val uses_similar = ref false

local
    val url_prefixes = ref []
in

fun reset () = (url_prefixes := []; uses_similar := false)

fun addPrefix prefix =
    let
        fun isPrefix s1 s2 =
            String.isPrefix s1 s2
            andalso (size s1 = size s2
                     orelse String.sub (s2, size s1) = #"/")
    in
        if List.exists (fn prefix' =>
                           let
                               fun tryOne prefix' prefix =
                                   isPrefix prefix' prefix
                                   andalso (ErrorMsg.error ("Conflicting URL prefixes for page handlers: \"" ^ prefix' ^ "\" is a prefix of \"" ^ prefix ^ "\".");
                                            true)
                           in
                               tryOne prefix' prefix
                               orelse tryOne prefix prefix'
                           end) (!url_prefixes) then
            ()
        else
            url_prefixes := prefix :: !url_prefixes
    end

end
                          
val nextPvar = MonoFooify.nextPvar
val pvars = ref (RM.empty : (int * (string * int * L'.typ) list) RM.map)
val pvarDefs = MonoFooify.pvarDefs
val pvarOldDefs = ref ([] : (int * (string * int * L.con option) list) list)

fun choosePvar () =
    let
        val n = !nextPvar
    in
        nextPvar := n + 1;
        n
    end

fun pvar (r, r', loc) =
    case RM.find (!pvars, r') of
        NONE =>
        let
            val n = choosePvar ()
            val fs = map (fn (x, t) => (x, choosePvar (), t)) r'
            val r = ListMergeSort.sort (fn (((L.CName x, _), _), ((L.CName y, _), _)) => String.compare (x, y) = GREATER
                                         | _ => raise Fail "Monoize: pvar, not CName") r
            val (r, fs') = ListPair.foldr (fn ((_, t), (x, n, _), (r, fs')) =>
                                              ((x, n, SOME t) :: r,
                                               SM.insert (fs', x, n))) ([], SM.empty) (r, fs)
        in
            pvars := RM.insert (!pvars, r', (n, fs));
            pvarDefs := ("$poly" ^ Int.toString n, n, map (fn (x, n, t) => (x, n, SOME t)) fs)
                        :: !pvarDefs;
            pvarOldDefs := (n, r) :: !pvarOldDefs;
            (n, fs)
        end
      | SOME v => v

val singletons = SS.addList (SS.empty,
                             ["link",
                              "br",
                              "p",
                              "hr",
                              "input",
                              "img",
                              "base",
                              "meta",
                              "param",
                              "area",
                              "col"])

val dummyTyp = (L'.TDatatype (0, ref (L'.Enum, [])), E.dummySpan)

structure U = MonoUtil

val liftExpInExp =
    U.Exp.mapB {typ = fn t => t,
                exp = fn bound => fn e =>
                                     case e of
                                         L'.ERel xn =>
                                         if xn < bound then
                                             e
                                         else
                                             L'.ERel (xn + 1)
                                       | _ => e,
                bind = fn (bound, U.Exp.RelE _) => bound + 1
                        | (bound, _) => bound}

fun monoName env (all as (c, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported name constructor";
             Print.eprefaces' [("Constructor", CorePrint.p_con env all)];
             "")
    in
        case c of
            L.CName s => s
          | _ => poly ()
    end

fun lowercaseFirst "" = ""
  | lowercaseFirst s = String.str (Char.toLower (String.sub (s, 0)))
                       ^ String.extract (s, 1, NONE)

fun monoNameLc env c = lowercaseFirst (monoName env c)

fun readType' (t, loc) = (L'.TFun ((L'.TFfi ("Basis", "string"), loc),
                                   (L'.TOption t, loc)), loc)
fun readErrType (t, loc) = (L'.TFun ((L'.TFfi ("Basis", "string"), loc),
                                     t), loc)
fun readType (t, loc) =
    (L'.TRecord [("Read", readType' (t, loc)),
                 ("ReadError", readErrType (t, loc))],
     loc)

fun monoType env =
    let
        fun mt env dtmap (all as (c, loc)) =
            let
                fun poly () =
                    (E.errorAt loc "Unsupported type constructor";
                     Print.eprefaces' [("Constructor", CorePrint.p_con env all)];
                     dummyTyp)
            in
                case c of
                    L.TFun (c1, c2) => (L'.TFun (mt env dtmap c1, mt env dtmap c2), loc)
                  | L.TCFun _ => poly ()
                  | L.TRecord (L.CRecord ((L.KType, _), xcs), _) =>
                    let
                        val xcs = map (fn (x, t) => (monoName env x, mt env dtmap t)) xcs
                        val xcs = ListMergeSort.sort (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) xcs
                    in
                        (L'.TRecord xcs, loc)
                    end
                  | L.TRecord _ => poly ()

                  | L.CApp ((L.CFfi ("Basis", "option"), _), t) =>
                    (L'.TOption (mt env dtmap t), loc)
                  | L.CApp ((L.CFfi ("Basis", "list"), _), t) =>
                    (L'.TList (mt env dtmap t), loc)

                  | L.CApp ((L.CFfi ("Basis", "variant"), _), (L.CRecord ((L.KType, _), xts), _)) =>
                    let
                        val xts' = map (fn (x, t) => (monoName env x, mt env dtmap t)) xts
                        val xts' = ListMergeSort.sort (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) xts'
                        val (n, cs) = pvar (xts, xts', loc)
                        val cs = map (fn (x, n, t) => (x, n, SOME t)) cs
                    in
                        (L'.TDatatype (n, ref (ElabUtil.classifyDatatype cs, cs)), loc)
                    end

                  | L.CApp ((L.CFfi ("Basis", "monad"), _), _) =>
                    (L'.TRecord [], loc)

                  | L.CApp ((L.CFfi ("Basis", "eq"), _), t) =>
                    let
                        val t = mt env dtmap t
                    in
                        (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc)
                    end
                  | L.CApp ((L.CFfi ("Basis", "num"), _), t) =>
                    let
                        val t = mt env dtmap t
                    in
                        (L'.TRecord [("Zero", t),
                                     ("Neg", (L'.TFun (t, t), loc)),
                                     ("Plus", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                                     ("Minus", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                                     ("Times", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                                     ("Div", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                                     ("Mod", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
				     ("Pow", (L'.TFun (t, (L'.TFun (t, t), loc)), loc))],
                         loc)
                    end
                  | L.CApp ((L.CFfi ("Basis", "ord"), _), t) =>
                    let
                        val t = mt env dtmap t
                    in
                        (L'.TRecord [("Lt", (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc)),
                                     ("Le", (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc))],
                         loc)
                    end
                  | L.CApp ((L.CFfi ("Basis", "show"), _), t) =>
                    (L'.TFun (mt env dtmap t, (L'.TFfi ("Basis", "string"), loc)), loc)
                  | L.CApp ((L.CFfi ("Basis", "read"), _), t) =>
                    readType (mt env dtmap t, loc)

                  | L.CFfi ("Basis", "unit") => (L'.TRecord [], loc)
                  | L.CFfi ("Basis", "page") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "xhead") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "xbody") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "xtable") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "xtr") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "xform") => (L'.TFfi ("Basis", "string"), loc)

                  | L.CFfi ("Basis", "url") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "mimeType") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "xml"), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "xhtml"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "css_class") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "css_value") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "css_property") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "css_style") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "id") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "requestHeader") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "responseHeader") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "envVar") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "meta") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "data_attr_kind") => (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "data_attr") => (L'.TFfi ("Basis", "string"), loc)

                  | L.CApp ((L.CFfi ("Basis", "serialized"), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)

                  | L.CApp ((L.CFfi ("Basis", "transaction"), _), t) =>
                    (L'.TFun ((L'.TRecord [], loc), mt env dtmap t), loc)
                  | L.CApp ((L.CFfi ("Basis", "source"), _), t) =>
                    (L'.TSource, loc)
                  | L.CApp ((L.CFfi ("Basis", "signal"), _), t) =>
                    (L'.TSignal (mt env dtmap t), loc)
                  | L.CApp ((L.CFfi ("Basis", "http_cookie"), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_table"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_view"), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "sql_sequence") =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_query"), _), _), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_query1"), _), _), _), _), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_from_items"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_exp"), _), _), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_expw"), _), _), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_window"), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_window_function"), _), _), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "primary_key"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_constraints"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "sql_constraints"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_constraint"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "linkable"), _), _), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "matching"), _), _), _), _) =>
                    let
                        val string = (L'.TFfi ("Basis", "string"), loc)
                    in
                        (L'.TRecord [("1", string), ("2", string)], loc)
                    end
                  | L.CApp ((L.CFfi ("Basis", "propagation_mode"), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "dml") =>
                    (L'.TFfi ("Basis", "string"), loc)

                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_subset"), _), _), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CFfi ("Basis", "sql_relop") =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "sql_direction") =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_order_by"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "sql_limit") =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "sql_offset") =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "fieldsOf"), _), _), _), _) =>
                    (L'.TRecord [], loc)

                  | L.CApp ((L.CFfi ("Basis", "sql_injectable_prim"), _), t) =>
                    (L'.TFun (mt env dtmap t, (L'.TFfi ("Basis", "string"), loc)), loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_injectable"), _), t) =>
                    (L'.TFun (mt env dtmap t, (L'.TFfi ("Basis", "string"), loc)), loc)
                  | L.CApp ((L.CFfi ("Basis", "trigrammable"), _), t) =>
                    (L'.TFun (mt env dtmap t, (L'.TRecord [], loc)), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "nullify"), _), _), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_unary"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_binary"), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_aggregate"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_summable"), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_maxable"), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_arith"), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_nfunc"), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_ufunc"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_bfunc"), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_partition"), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_window"), _), _), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)

                  | L.CApp ((L.CFfi ("Basis", "channel"), _), _) =>
                    (L'.TFfi ("Basis", "channel"), loc)

                  | L.CRel _ => poly ()
                  | L.CNamed n =>
                    (case IM.find (dtmap, n) of
                         SOME r => (L'.TDatatype (n, r), loc)
                       | NONE =>
                         let
                             val r = ref (L'.Default, [])
                             val (_, xs, xncs) = Env.lookupDatatype env n

                             val dtmap' = IM.insert (dtmap, n, r)

                             val xncs = map (fn (x, n, to) => (x, n, Option.map (mt env dtmap') to)) xncs
                         in
                             case xs of
                                 [] =>(r := (ElabUtil.classifyDatatype xncs, xncs);
                                       (L'.TDatatype (n, r), loc))
                               | _ => poly ()
                         end)
                  | L.CFfi mx => (L'.TFfi mx, loc)
                  | L.CApp _ => poly ()
                  | L.CAbs _ => poly ()

                  | L.CName _ => poly ()

                  | L.CRecord _ => poly ()
                  | L.CConcat _ => poly ()
                  | L.CMap _ => poly ()
                  | L.CUnit => poly ()

                  | L.CTuple _ => poly ()
                  | L.CProj _ => poly ()

                  | L.CKAbs _ => poly ()
                  | L.CKApp _ => poly ()
                  | L.TKFun _ => poly ()
            end
    in
        mt env IM.empty
    end

val dummyExp = (L'.EPrim (Prim.Int 0), E.dummySpan)

structure Fm = MonoFooify.Fm

fun fooifyExp fk env =
    MonoFooify.fooifyExp
        fk
        (fn n =>
            let
                val (_, t, _, s) = Env.lookupENamed env n
            in
                (monoType env t, s)
            end)
        (fn n =>
            let
                val (x, _, xncs) = Env.lookupDatatype env n
            in
                (x, map (fn (x, n, c) => (x, n, Option.map (monoType env) c)) xncs)
            end)

val attrifyExp = fooifyExp MonoFooify.Attr
val urlifyExp = fooifyExp MonoFooify.Url

datatype 'a failable_search =
         Found of 'a
       | NotFound
       | Error

structure St :> sig
    type t

    val empty : t

    val radioGroup : t -> string option
    val setRadioGroup : t * string -> t
end = struct

type t = {
     radioGroup : string option
}

val empty = {radioGroup = NONE}

fun radioGroup (t : t) = #radioGroup t

fun setRadioGroup (t : t, x) = {radioGroup = SOME x}

end

fun monoPatCon env pc =
    case pc of
        L.PConVar n => L'.PConVar n
      | L.PConFfi {mod = m, datatyp, con, arg, ...} => L'.PConFfi {mod = m, datatyp = datatyp, con = con,
                                                                   arg = Option.map (monoType env) arg}

val dummyPat = (L'.PPrim (Prim.Int 0), ErrorMsg.dummySpan)


fun listify t = (L'.TRecord [("1", t), ("2", (L'.TList t, #2 t))], #2 t)

fun monoPat env (all as (p, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported pattern";
             Print.eprefaces' [("Pattern", CorePrint.p_pat env all)];
             dummyPat)
    in
        case p of
            L.PVar (x, t) => (L'.PVar (x, monoType env t), loc)
          | L.PPrim p => (L'.PPrim p, loc)
          | L.PCon (dk, pc, [], po) => (L'.PCon (dk, monoPatCon env pc, Option.map (monoPat env) po), loc)
          | L.PCon (L.Option, L.PConFfi {mod = "Basis", datatyp = "list", ...}, [t], NONE) =>
            (L'.PNone (listify (monoType env t)), loc)
          | L.PCon (L.Option, L.PConFfi {mod = "Basis", datatyp = "list", ...}, [t], SOME p) =>
            (L'.PSome (listify (monoType env t), monoPat env p), loc)
          | L.PCon (L.Option, _, [t], NONE) => (L'.PNone (monoType env t), loc)
          | L.PCon (L.Option, pc, [t], SOME p) => (L'.PSome (monoType env t, monoPat env p), loc)
          | L.PCon _ => poly ()
          | L.PRecord xps => (L'.PRecord (map (fn (x, p, t) => (x, monoPat env p, monoType env t)) xps), loc)
    end

fun strcat loc es =
    case es of
        [] => (L'.EPrim (Prim.String (Prim.Normal, "")), loc)
      | [e] => e
      | _ =>
        let
            val e2 = List.last es
            val es = List.take (es, length es - 1)
            val e1 = List.last es
            val es = List.take (es, length es - 1)
        in
            foldr (fn (e, e') => (L'.EStrcat (e, e'), loc))
            (L'.EStrcat (e1, e2), loc) es
        end

fun strcatComma loc es =
    case es of
        [] => (L'.EPrim (Prim.String (Prim.Normal, "")), loc)
      | [e] => e
      | _ =>
        let
            val e1 = List.last es
            val es = List.take (es, length es - 1)
        in
            foldr (fn (e, e') =>
                      case (e, e') of
                          ((L'.EPrim (Prim.String (_, "")), _), _) => e'
                        | (_, (L'.EPrim (Prim.String (_, "")), _)) => e
                        | _ =>
                          (L'.EStrcat (e,
                                       (L'.EStrcat ((L'.EPrim (Prim.String (Prim.Normal, ", ")), loc), e'), loc)), loc))
            e1 es
        end

fun strcatR loc e xs = strcatComma loc (map (fn (x, _) => (L'.EField (e, x), loc)) xs)

val readCookie = ref IS.empty

fun isBlobby (t : L.con) =
    case #1 t of
        L.CFfi ("Basis", "string") => true
      | L.CFfi ("Basis", "blob") => true
      | _ => false

fun monoExp (env, st, fm) (all as (e, loc)) =
    let
        val strcat = strcat loc
        val strcatComma = strcatComma loc
        fun str s = (L'.EPrim (Prim.String (Prim.Normal, s)), loc)
        fun strH s = (L'.EPrim (Prim.String (Prim.Html, s)), loc)

        fun poly () =
            (E.errorAt loc "Unsupported expression";
             Print.eprefaces' [("Expression", CorePrint.p_exp env all)];
             (dummyExp, fm))

        fun numTy t =
            (L'.TRecord [("Zero", t),
                         ("Neg", (L'.TFun (t, t), loc)),
                         ("Plus", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                         ("Minus", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                         ("Times", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                         ("Div", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                         ("Mod", (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
			 ("Pow", (L'.TFun (t, (L'.TFun (t, t), loc)), loc))], loc)
        fun numEx (t, zero, neg, plus, minus, times, dv, md, ex) =
            ((L'.ERecord [("Zero", (L'.EPrim zero, loc), t),
                          ("Neg", neg, (L'.TFun (t, t), loc)),
                          ("Plus", plus, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Minus", minus, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Times", times, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Div", dv, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Mod", md, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Pow", ex, (L'.TFun (t, (L'.TFun (t, t), loc)), loc))], loc), fm)

        fun ordTy t =
            (L'.TRecord [("Lt", (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc)),
                         ("Le", (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc))], loc)
        fun ordEx (t, lt, le) =
            ((L'.ERecord [("Lt", lt, (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc)),
                          ("Le", le, (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc))],
              loc), fm)

        fun outerRec xts =
            (L'.TRecord (map (fn ((L.CName x, _), (L.CRecord (_, xts), _)) =>
                                 (x, (L'.TRecord (map (fn (x', _) => (x, (L'.TRecord [], loc))) xts), loc))
                               | (x, all as (_, loc)) =>
                                 (E.errorAt loc "Unsupported record field constructor";
                                  Print.eprefaces' [("Name", CorePrint.p_con env x),
                                                    ("Constructor", CorePrint.p_con env all)];
                                  ("", dummyTyp))) xts), loc)
    in
        case e of
            L.EPrim p => ((L'.EPrim p, loc), fm)
          | L.ERel n => ((L'.ERel n, loc), fm)
          | L.ENamed n => ((L'.ENamed n, loc), fm)
          | L.ECon (dk, pc, [], eo) =>
            let
                val (eo, fm) =
                    case eo of
                        NONE => (NONE, fm)
                      | SOME e =>
                        let
                            val (e, fm) = monoExp (env, st, fm) e
                        in
                            (SOME e, fm)
                        end
            in
                ((L'.ECon (dk, monoPatCon env pc, eo), loc), fm)
            end
          | L.ECon (L.Option, L.PConFfi {mod = "Basis", datatyp = "list", ...}, [t], NONE) =>
            ((L'.ENone (listify (monoType env t)), loc), fm)
          | L.ECon (L.Option, L.PConFfi {mod = "Basis", datatyp = "list", ...}, [t], SOME e) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.ESome (listify (monoType env t), e), loc), fm)
            end
          | L.ECon (L.Option, _, [t], NONE) =>
            ((L'.ENone (monoType env t), loc), fm)
          | L.ECon (L.Option, _, [t], SOME e) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.ESome (monoType env t, e), loc), fm)
            end
          | L.ECon _ => poly ()

          | L.ECApp (
            (L.ECApp (
             (L.ECApp ((L.EFfi ("Basis", "make"), _), nmC as (L.CName nm, _)), _),
             t), _),
            (L.CRecord (_, xts), _)) =>
            let
                val t' = monoType env t
                val xts' = map (fn (x, t) => (monoName env x, monoType env t)) xts
                val xts' = (nm, t') :: xts'
                val xts' = ListMergeSort.sort (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) xts'
                val (n, cs) = pvar ((nmC, t) :: xts, xts', loc)
                val cs' = map (fn (x, n, t) => (x, n, SOME t)) cs
                val cl = ElabUtil.classifyDatatype cs'
            in
                case List.find (fn (nm', _, _) => nm' = nm) cs of
                    NONE => raise Fail "Monoize: Polymorphic variant tag mismatch for 'make'"
                  | SOME (_, n', _) => ((L'.EAbs ("x", t', (L'.TDatatype (n, ref (cl, cs')), loc),
                                                  (L'.ECon (cl, L'.PConVar n', SOME (L'.ERel 0, loc)), loc)), loc),
                                        fm)
            end

          | L.ECApp (
            (L.ECApp ((L.EFfi ("Basis", "match"), _), (L.CRecord (_, xts), _)), _),
            t) =>
            let
                val t = monoType env t
                val xts' = map (fn (x, t) => (monoName env x, monoType env t)) xts
                val xts' = ListMergeSort.sort (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) xts'
                val (n, cs) = pvar (xts, xts', loc)
                val cs' = map (fn (x, n, t) => (x, n, SOME t)) cs
                val cl = ElabUtil.classifyDatatype cs'
                val fs = (L'.TRecord (map (fn (x, t') => (x, (L'.TFun (t', t), loc))) xts'), loc)
                val dt = (L'.TDatatype (n, ref (cl, cs')), loc)
            in
                ((L'.EAbs ("v",
                           dt,
                           (L'.TFun (fs, t), loc),
                           (L'.EAbs ("fs", fs, t,
                                     (L'.ECase ((L'.ERel 1, loc),
                                                map (fn (x, n', t') =>
                                                        ((L'.PCon (cl, L'.PConVar n', SOME (L'.PVar ("x", t'), loc)), loc),
                                                         (L'.EApp ((L'.EField ((L'.ERel 1, loc), x), loc),
                                                                   (L'.ERel 0, loc)), loc))) cs,
                                                {disc = dt, result = t}), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "eq"), _), t) =>
            let
                val t = monoType env t
                val b = (L'.TFfi ("Basis", "bool"), loc)
                val dom = (L'.TFun (t, (L'.TFun (t, b), loc)), loc)
            in
                ((L'.EAbs ("f", dom, dom,
                           (L'.ERel 0, loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "ne"), _), t) =>
            let
                val t = monoType env t
                val b = (L'.TFfi ("Basis", "bool"), loc)
                val dom = (L'.TFun (t, (L'.TFun (t, b), loc)), loc)
            in
                ((L'.EAbs ("f", dom, dom,
                           (L'.EAbs ("x", t, (L'.TFun (t, b), loc),
                                     (L'.EAbs ("y", t, b,
                                               (L'.EUnop ("!", (L'.EApp ((L'.EApp ((L'.ERel 2, loc),
                                                                                   (L'.ERel 1, loc)), loc),
                                                                         (L'.ERel 0, loc)), loc)), loc)),
                                      loc)),
                            loc)),
                  loc), fm)
            end
          | L.EFfi ("Basis", "eq_int") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "int"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "int"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "int"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop (L'.Int, "==", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_float") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "float"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "float"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop (L'.NotInt, "==", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_bool") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "bool"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "bool"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop (L'.NotInt, "==", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_string") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "string"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "string"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "string"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop (L'.NotInt, "!strcmp", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_char") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "char"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "char"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "char"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop (L'.NotInt, "==", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_time") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "time"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "time"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "time"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EFfiApp ("Basis", "eq_time", [((L'.ERel 1, loc), (L'.TFfi ("Basis", "time"), loc)),
                                                                   ((L'.ERel 0, loc), (L'.TFfi ("Basis", "time"), loc))]), loc)), loc)), loc),
             fm)

          | L.ECApp ((L.EFfi ("Basis", "mkEq"), _), t) =>
            let
                val t = monoType env t
                val b = (L'.TFfi ("Basis", "bool"), loc)
                val dom = (L'.TFun (t, (L'.TFun (t, b), loc)), loc)
            in
                ((L'.EAbs ("f", dom, dom,
                           (L'.ERel 0, loc)), loc), fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "zero"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", numTy t, t,
                           (L'.EField ((L'.ERel 0, loc), "Zero"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "neg"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", numTy t, (L'.TFun (t, t), loc),
                           (L'.EField ((L'.ERel 0, loc), "Neg"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "plus"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", numTy t, (L'.TFun (t, (L'.TFun (t, t), loc)), loc),
                           (L'.EField ((L'.ERel 0, loc), "Plus"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "minus"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", numTy t, (L'.TFun (t, (L'.TFun (t, t), loc)), loc),
                           (L'.EField ((L'.ERel 0, loc), "Minus"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "times"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", numTy t, (L'.TFun (t, (L'.TFun (t, t), loc)), loc),
                           (L'.EField ((L'.ERel 0, loc), "Times"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "divide"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", numTy t, (L'.TFun (t, (L'.TFun (t, t), loc)), loc),
                           (L'.EField ((L'.ERel 0, loc), "Div"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "mod"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", numTy t, (L'.TFun (t, (L'.TFun (t, t), loc)), loc),
                           (L'.EField ((L'.ERel 0, loc), "Mod"), loc)), loc), fm)
            end
	  | L.ECApp ((L.EFfi ("Basis", "pow"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", numTy t, (L'.TFun (t, (L'.TFun (t, t), loc)), loc),
                           (L'.EField ((L'.ERel 0, loc), "Pow"), loc)), loc), fm)
            end
          | L.EFfi ("Basis", "num_int") =>
            let
                fun intBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "int"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "int"), loc), (L'.TFfi ("Basis", "int"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "int"), loc),
                                        (L'.TFfi ("Basis", "int"), loc),
                                        (L'.EBinop (L'.Int, s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                numEx ((L'.TFfi ("Basis", "int"), loc),
                       Prim.Int (Int64.fromInt 0),
                       (L'.EAbs ("x", (L'.TFfi ("Basis", "int"), loc),
                                 (L'.TFfi ("Basis", "int"), loc),
                                 (L'.EUnop ("-", (L'.ERel 0, loc)), loc)), loc),
                       intBin "+",
                       intBin "-",
                       intBin "*",
                       intBin "/",
                       intBin "%",
                       intBin "powl"
                       )
            end
          | L.EFfi ("Basis", "num_float") =>
            let
                fun floatBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "float"), loc), (L'.TFfi ("Basis", "float"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "float"), loc),
                                        (L'.TFfi ("Basis", "float"), loc),
                                        (L'.EBinop (L'.NotInt, s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                numEx ((L'.TFfi ("Basis", "float"), loc),
                       Prim.Float 0.0,
                       (L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc),
                                 (L'.TFfi ("Basis", "float"), loc),
                                 (L'.EUnop ("-", (L'.ERel 0, loc)), loc)), loc),
                       floatBin "+",
                       floatBin "-",
                       floatBin "*",
                       floatBin "fdiv",
                       floatBin "fmod",
                       floatBin "powf"
		       )
            end

          | L.ECApp ((L.EFfi ("Basis", "lt"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", ordTy t, (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc),
                           (L'.EField ((L'.ERel 0, loc), "Lt"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "le"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("r", ordTy t, (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc),
                           (L'.EField ((L'.ERel 0, loc), "Le"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "gt"), _), t) =>
            let
                val t = monoType env t
                val b = (L'.TFfi ("Basis", "bool"), loc)
            in
                ((L'.EAbs ("f", ordTy t, (L'.TFun (t, (L'.TFun (t, b), loc)), loc),
                           (L'.EAbs ("x", t, (L'.TFun (t, b), loc),
                                     (L'.EAbs ("y", t, b,
                                               (L'.EUnop ("!",
                                                          (L'.EApp ((L'.EApp ((L'.EField ((L'.ERel 2, loc),
                                                                                          "Le"), loc),
                                                                              (L'.ERel 1, loc)), loc),
                                                                    (L'.ERel 0, loc)), loc)), loc)), loc)),
                            loc)),
                  loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "ge"), _), t) =>
            let
                val t = monoType env t
                val b = (L'.TFfi ("Basis", "bool"), loc)
            in
                ((L'.EAbs ("f", ordTy t, (L'.TFun (t, (L'.TFun (t, b), loc)), loc),
                           (L'.EAbs ("x", t, (L'.TFun (t, b), loc),
                                     (L'.EAbs ("y", t, b,
                                               (L'.EUnop ("!",
                                                          (L'.EApp ((L'.EApp ((L'.EField ((L'.ERel 2, loc),
                                                                                          "Lt"), loc),
                                                                              (L'.ERel 1, loc)), loc),
                                                                    (L'.ERel 0, loc)), loc)), loc)), loc)),
                            loc)),
                  loc), fm)
            end
          | L.EFfi ("Basis", "ord_int") =>
            let
                fun intBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "int"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "int"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "int"), loc),
                                        (L'.TFfi ("Basis", "bool"), loc),
                                        (L'.EBinop (L'.Int, s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                ordEx ((L'.TFfi ("Basis", "int"), loc),
                       intBin "<",
                       intBin "<=")
            end
          | L.EFfi ("Basis", "ord_float") =>
            let
                fun floatBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "float"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "float"), loc),
                                        (L'.TFfi ("Basis", "bool"), loc),
                                        (L'.EBinop (L'.NotInt, s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                ordEx ((L'.TFfi ("Basis", "float"), loc),
                       floatBin "<",
                       floatBin "<=")
            end
          | L.EFfi ("Basis", "ord_bool") =>
            let
                fun boolBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "bool"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "bool"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "bool"), loc),
                                        (L'.TFfi ("Basis", "bool"), loc),
                                        (L'.EBinop (L'.NotInt, s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                ordEx ((L'.TFfi ("Basis", "bool"), loc),
                       boolBin "<",
                       boolBin "<=")
            end
          | L.EFfi ("Basis", "ord_string") =>
            let
                fun boolBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "string"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "string"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "string"), loc),
                                        (L'.TFfi ("Basis", "bool"), loc),
                                        (L'.EBinop (L'.NotInt, s,
                                                    (L'.EBinop (L'.NotInt, "strcmp",
                                                                (L'.ERel 1, loc),
                                                                (L'.ERel 0, loc)), loc),
                                                    (L'.EPrim (Prim.Int (Int64.fromInt 0)), loc)), loc)), loc)), loc)
            in
                ordEx ((L'.TFfi ("Basis", "string"), loc),
                       boolBin "<",
                       boolBin "<=")
            end
          | L.EFfi ("Basis", "ord_char") =>
            let
                fun charBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "char"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "char"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "char"), loc),
                                        (L'.TFfi ("Basis", "bool"), loc),
                                        (L'.EBinop (L'.NotInt, s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                ordEx ((L'.TFfi ("Basis", "char"), loc),
                       charBin "<",
                       charBin "<=")
            end
          | L.EFfi ("Basis", "ord_time") =>
            let
                fun boolBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "time"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "time"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "time"), loc),
                                        (L'.TFfi ("Basis", "bool"), loc),
                                        (L'.EFfiApp ("Basis", s, [((L'.ERel 1, loc), (L'.TFfi ("Basis", "time"), loc)),
                                                                  ((L'.ERel 0, loc), (L'.TFfi ("Basis", "time"), loc))]), loc)), loc)), loc)
            in
                ordEx ((L'.TFfi ("Basis", "time"), loc),
                       boolBin "lt_time",
                       boolBin "le_time")
            end
          | L.ECApp ((L.EFfi ("Basis", "mkOrd"), _), t) =>
            let
                val t = monoType env t
                val b = (L'.TFfi ("Basis", "bool"), loc)
                val dom = ordTy t
            in
                ((L'.EAbs ("f", dom, dom,
                           (L'.ERel 0, loc)), loc), fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "show"), _), t) =>
            let
                val t = monoType env t
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("f", (L'.TFun (t, s), loc), (L'.TFun (t, s), loc),
                           (L'.ERel 0, loc)), loc), fm)
            end
          | L.EFfi ("Basis", "show_int") =>
            ((L'.EFfi ("Basis", "intToString"), loc), fm)
          | L.EFfi ("Basis", "show_float") =>
            ((L'.EFfi ("Basis", "floatToString"), loc), fm)
          | L.EFfi ("Basis", "show_string") =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc), fm)
            end
          | L.EFfi ("Basis", "show_queryString") =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc), fm)
            end
          | L.EFfi ("Basis", "show_url") =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc), fm)
            end
          | L.EFfi ("Basis", "show_css_class") =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc), fm)
            end
          | L.EFfi ("Basis", "show_id") =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc), fm)
            end
          | L.EFfi ("Basis", "show_char") =>
            ((L'.EFfi ("Basis", "charToString"), loc), fm)
          | L.EFfi ("Basis", "show_bool") =>
            ((L'.EFfi ("Basis", "boolToString"), loc), fm)
          | L.EFfi ("Basis", "show_time") =>
            ((L'.EFfi ("Basis", "timeToString"), loc), fm)
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "show_xml"), _), _),_), _), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc), fm)
            end
          | L.ECApp ((L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "show_sql_query"), _), _), _), _), _), _), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "mkShow"), _), t) =>
            let
                val t = monoType env t
                val b = (L'.TFfi ("Basis", "string"), loc)
                val dom = (L'.TFun (t, b), loc)
            in
                ((L'.EAbs ("f", dom, dom,
                           (L'.ERel 0, loc)), loc), fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "read"), _), t) =>
            let
                val t = monoType env t
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("f", readType (t, loc), readType' (t, loc),
                           (L'.EField ((L'.ERel 0, loc), "Read"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "readError"), _), t) =>
            let
                val t = monoType env t
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("f", readType (t, loc), readErrType (t, loc),
                           (L'.EField ((L'.ERel 0, loc), "ReadError"), loc)), loc), fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "mkRead"), _), t) =>
            let
                val t = monoType env t
                val b = (L'.TFfi ("Basis", "string"), loc)
                val b' = (L'.TOption b, loc)
                val dom = (L'.TFun (t, b), loc)
                val dom' = (L'.TFun (t, b'), loc)
            in
                ((L'.EAbs ("f", dom, (L'.TFun (dom', readType (t, loc)), loc),
                           (L'.EAbs ("f'", dom', readType (t, loc),
                                     (L'.ERecord [("Read", (L'.ERel 0, loc), dom),
                                                  ("ReadError", (L'.ERel 1, loc), dom')], loc)), loc)), loc),
                 fm)
            end
          | L.EFfi ("Basis", "read_int") =>
            let
                val t = (L'.TFfi ("Basis", "int"), loc)
            in
                ((L'.ERecord [("Read", (L'.EFfi ("Basis", "stringToInt"), loc), readType' (t, loc)),
                               ("ReadError", (L'.EFfi ("Basis", "stringToInt_error"), loc), readErrType (t, loc))],
                  loc),
                 fm)
            end
          | L.EFfi ("Basis", "read_float") =>
            let
                val t = (L'.TFfi ("Basis", "float"), loc)
            in
                ((L'.ERecord [("Read", (L'.EFfi ("Basis", "stringToFloat"), loc), readType' (t, loc)),
                              ("ReadError", (L'.EFfi ("Basis", "stringToFloat_error"), loc), readErrType (t, loc))],
                  loc),
                 fm)
            end
          | L.EFfi ("Basis", "read_string") =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.ERecord [("Read", (L'.EAbs ("s", s, (L'.TOption s, loc),
                                                 (L'.ESome (s, (L'.ERel 0, loc)), loc)), loc), readType' (s, loc)),
                              ("ReadError", (L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc), readErrType (s, loc))], loc),
                 fm)
            end
          | L.EFfi ("Basis", "read_char") =>
            let
                val t = (L'.TFfi ("Basis", "char"), loc)
            in
                ((L'.ERecord [("Read", (L'.EFfi ("Basis", "stringToChar"), loc), readType' (t, loc)),
                               ("ReadError", (L'.EFfi ("Basis", "stringToChar_error"), loc), readErrType (t, loc))],
                  loc),
                 fm)
            end
          | L.EFfi ("Basis", "read_bool") =>
            let
                val t = (L'.TFfi ("Basis", "bool"), loc)
            in
                ((L'.ERecord [("Read", (L'.EFfi ("Basis", "stringToBool"), loc), readType' (t, loc)),
                              ("ReadError", (L'.EFfi ("Basis", "stringToBool_error"), loc), readErrType (t, loc))],
                  loc),
                 fm)
            end
          | L.EFfi ("Basis", "read_time") =>
            let
                val t = (L'.TFfi ("Basis", "time"), loc)
            in
                ((L'.ERecord [("Read", (L'.EFfi ("Basis", "stringToTime"), loc), readType' (t, loc)),
                              ("ReadError", (L'.EFfi ("Basis", "stringToTime_error"), loc), readErrType (t, loc))],
                  loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "transaction_return"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("x", t,
                                     (L'.TFun ((L'.TRecord [], loc), t), loc),
                                     (L'.EAbs ("_", (L'.TRecord [], loc), t,
                                               (L'.ERel 1, loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "transaction_bind"), _), t1), _), t2) =>
            let
                val t1 = monoType env t1
                val t2 = monoType env t2
                val un = (L'.TRecord [], loc)
                val mt1 = (L'.TFun (un, t1), loc)
                val mt2 = (L'.TFun (un, t2), loc)
            in
                ((L'.EAbs ("m1", mt1, (L'.TFun ((L'.TFun (t1, mt2), loc), (L'.TFun (un, un), loc)), loc),
                           (L'.EAbs ("m2", (L'.TFun (t1, mt2), loc), (L'.TFun (un, un), loc),
                                     (L'.EAbs ("_", un, un,
                                               (L'.ELet ("r", t1, (L'.EApp ((L'.ERel 2, loc),
                                                                            (L'.ERecord [], loc)), loc),
                                                         (L'.EApp (
                                                          (L'.EApp ((L'.ERel 2, loc), (L'.ERel 0, loc)), loc),
                                                          (L'.ERecord [], loc)),
                                                          loc)), loc)), loc)), loc)), loc),
                 fm)
            end

          | L.EApp ((L.ECApp ((L.EFfi ("Basis", "recv"), _), t1), _), ch) =>
            let
                val un = (L'.TRecord [], loc)
                val t1 = monoType env t1
                val (ch, fm) = monoExp (env, st, fm) ch
            in
                ((L'.EAbs ("_", un, un, (L'.ERecv (liftExpInExp 0 ch, t1), loc)), loc), fm)
            end
          | L.EFfiApp ("Basis", "recv", _) => poly ()

          | L.EFfiApp ("Basis", "float", [(e, t)]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.EFfiApp ("Basis", "floatFromInt", [(e, monoType env t)]), loc), fm)
            end

          | L.EFfiApp ("Basis", "sleep", [(n, _)]) =>
            let
                val (n, fm) = monoExp (env, st, fm) n
            in
                ((L'.ESleep n, loc), fm)
            end
          | L.EFfiApp ("Basis", "sleep", _) => poly ()

          | L.ECApp ((L.EFfi ("Basis", "source"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("x", t, (L'.TFun ((L'.TRecord [], loc), (L'.TSource, loc)), loc),
                           (L'.EAbs ("_", (L'.TRecord [], loc), (L'.TSource, loc),
                                     (L'.EFfiApp ("Basis", "new_client_source",
                                                  [((L'.EJavaScript (L'.Source t, (L'.ERel 1, loc)), loc),
                                                    (L'.TSource, loc))]),
                                      loc)), loc)),
                  loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "set"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("src", (L'.TSource, loc),
                           (L'.TFun (t, (L'.TFun ((L'.TRecord [], loc), (L'.TRecord [], loc)), loc)), loc),
                           (L'.EAbs ("v", t, (L'.TFun ((L'.TRecord [], loc), (L'.TRecord [], loc)), loc),
                                     (L'.EAbs ("_", (L'.TRecord [], loc), (L'.TRecord [], loc),
                                               (L'.EFfiApp ("Basis", "set_client_source",
                                                            [((L'.ERel 2, loc), (L'.TSource, loc)),
                                                             ((L'.EJavaScript (L'.Source t,
                                                                               (L'.ERel 1, loc)), loc),
                                                              (L'.TFfi ("Basis", "string"), loc))]),
                                                loc)), loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "get"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("src", (L'.TSource, loc),
                           (L'.TFun ((L'.TRecord [], loc), t), loc),
                           (L'.EAbs ("_", (L'.TRecord [], loc), t,
                                     (L'.EFfiApp ("Basis", "get_client_source",
                                                  [((L'.ERel 1, loc), (L'.TSource, loc))]),
                                      loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "current"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("src", (L'.TSource, loc),
                           (L'.TFun ((L'.TRecord [], loc), t), loc),
                           (L'.EAbs ("_", (L'.TRecord [], loc), t,
                                     (L'.EFfiApp ("Basis", "current",
                                                  [((L'.ERel 1, loc), (L'.TSource, loc))]),
                                      loc)), loc)), loc),
                 fm)
            end

          | L.EFfiApp ("Basis", "spawn", [(e, _)]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.ESpawn e, loc), fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "signal_return"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("x", t, (L'.TSignal t, loc),
                           (L'.ESignalReturn (L'.ERel 0, loc), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "signal_bind"), _), t1), _), t2) =>
            let
                val t1 = monoType env t1
                val t2 = monoType env t2
                val un = (L'.TRecord [], loc)
                val mt1 = (L'.TSignal t1, loc)
                val mt2 = (L'.TSignal t2, loc)
            in
                ((L'.EAbs ("m1", mt1, (L'.TFun ((L'.TFun (t1, mt2), loc), mt2), loc),
                           (L'.EAbs ("m2", (L'.TFun (t1, mt2), loc), mt2,
                                     (L'.ESignalBind ((L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "signal"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("x", (L'.TFfi ("Basis", "int"), loc), (L'.TSignal t, loc),
                           (L'.ESignalSource (L'.ERel 0, loc), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "getCookie"), _), t) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val un = (L'.TRecord [], loc)
                val t = monoType env t
            in
                ((L'.EAbs ("c", s, (L'.TFun (un, s), loc),
                           (L'.EAbs ("_", un, s,
                                     (L'.EUnurlify ((L'.EFfiApp ("Basis", "get_cookie", [((L'.ERel 1, loc), s)]), loc),
                                                    t, true),
                                      loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "setCookie"), _), t) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val un = (L'.TRecord [], loc)
                val t = monoType env t
                val rt = (L'.TRecord [("Value", t),
                                      ("Expires", (L'.TOption (L'.TFfi ("Basis", "time"),
                                                               loc), loc)),
                                      ("Secure", (L'.TFfi ("Basis", "bool"), loc))], loc)

                fun fd x = (L'.EField ((L'.ERel 1, loc), x), loc)
                val (e, fm) = urlifyExp env fm (fd "Value", t)
            in
                ((L'.EAbs ("c", s, (L'.TFun (rt, (L'.TFun (un, un), loc)), loc),
                           (L'.EAbs ("r", rt, (L'.TFun (un, un), loc),
                                     (L'.EAbs ("_", un, un,
                                               (L'.EFfiApp ("Basis", "set_cookie", [(str (Settings.getUrlPrefix ()), s),
                                                                                    ((L'.ERel 2, loc), s),
                                                                                    (e, s),
                                                                                    (fd "Expires", (L'.TOption (L'.TFfi ("Basis", "time"), loc), loc)),
                                                                                    (fd "Secure", (L'.TFfi ("Basis", "bool"), loc))])
                                              , loc)), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "clearCookie"), _), t) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val un = (L'.TRecord [], loc)
            in
                ((L'.EAbs ("c", s, (L'.TFun (un, un), loc),
                           (L'.EAbs ("_", un, un,
                                     (L'.EFfiApp ("Basis", "clear_cookie",
                                                  [(str (Settings.getUrlPrefix ()), s),
                                                   ((L'.ERel 1, loc), s)]),
                                      loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "channel"), _), t) =>
                ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "channel"), loc),
                           (L'.EFfiApp ("Basis", "new_channel", [((L'.ERecord [], loc), (L'.TRecord [], loc))]), loc)), loc),
                 fm)
          | L.ECApp ((L.EFfi ("Basis", "send"), _), t) =>
            let
                val t = monoType env t
                val (e, fm) = urlifyExp env fm ((L'.ERel 1, loc), t)
            in
                ((L'.EAbs ("ch", (L'.TFfi ("Basis", "channel"), loc),
                           (L'.TFun (t, (L'.TFun ((L'.TRecord [], loc), (L'.TRecord [], loc)), loc)), loc),
                           (L'.EAbs ("v", t, (L'.TFun ((L'.TRecord [], loc), (L'.TRecord [], loc)), loc),
                                     (L'.EAbs ("_", (L'.TRecord [], loc), (L'.TRecord [], loc),
                                               (L'.EFfiApp ("Basis", "send",
                                                            [((L'.ERel 2, loc), (L'.TFfi ("Basis", "channel"), loc)),
                                                             (e, (L'.TFfi ("Basis", "string"), loc))]),
                                                loc)), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "no_primary_key"), _), _) =>
            (str "", fm)
          | L.ECApp (
            (L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "primary_key"), _), _), _), t), _),
                      nm), _),
            (L.CRecord (_, unique), _)) =>
            let
                val unique = (nm, t) :: unique
                val witnesses = (L'.TRecord (map (fn (nm, _) => (monoName env nm, (L'.TRecord [], loc))) unique), loc)
            in
                ((L'.EAbs ("_", witnesses, (L'.TFfi ("Basis", "string"), loc),
                           (str
                                (String.concatWith ", "
                                                   (map (fn (x, _) =>
                                                            Settings.mangleSql (monoNameLc env x)
                                                            ^ (if #textKeysNeedLengths (Settings.currentDbms ())
                                                                  andalso isBlobby t then
                                                                   "(255)"
                                                               else
                                                                   "")) unique)))),
                  loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "no_constraint"), _), _) =>
            ((L'.ERecord [], loc),
             fm)
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "one_constraint"), _), _), _), _), _), (L.CName name, _)) =>
            ((L'.EAbs ("c",
                       (L'.TFfi ("Basis", "string"), loc),
                       (L'.TFfi ("Basis", "sql_constraints"), loc),
                       (L'.ERecord [(name, (L'.ERel 0, loc), (L'.TFfi ("Basis", "string"), loc))], loc)), loc),
             fm)
          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.EFfi ("Basis", "join_constraints"), _),
              _), _),
             _), _),
            _) =>
            let
                val constraints = (L'.TFfi ("Basis", "sql_constraints"), loc)
            in
                ((L'.EAbs ("cs1", constraints, (L'.TFun (constraints, constraints), loc),
                           (L'.EAbs ("cs2", constraints, constraints,
                                     (L'.EStrcat ((L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp (
            (L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "unique"), _), _), _), t), _),
                      nm), _),
            (L.CRecord (_, unique), _)) =>
            let
                val unique = (nm, t) :: unique
            in
                (str ("UNIQUE ("
                      ^ String.concatWith ", "
                                          (map (fn (x, t) => Settings.mangleSql (monoNameLc env x)
                                                             ^ (if #textKeysNeedLengths (Settings.currentDbms ())
                                                                   andalso isBlobby t then
                                                                    "(255)"
                                                                else
                                                                    "")) unique)
                      ^ ")"),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "linkable_same"), loc), _) =>
            ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "linkable_from_nullable"), loc), _) =>
            ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "linkable_to_nullable"), loc), _) =>
            ((L'.ERecord [], loc), fm)

          | L.EFfi ("Basis", "mat_nil") =>
            let
                val string = (L'.TFfi ("Basis", "string"), loc)
                val stringE = str ""
            in
                ((L'.ERecord [("1", stringE, string),
                              ("2", stringE, string)], loc), fm)
            end
          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.EFfi ("Basis", "mat_cons"), _),
                 _), _),
                _), _),
               _), _),
              _), _),
             (L.CName nm1, _)), _),
            (L.CName nm2, _)) =>
            let
                val string = (L'.TFfi ("Basis", "string"), loc)
                val mat = (L'.TRecord [("1", string), ("2", string)], loc)
            in
                ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFun (mat, mat), loc),
                           (L'.EAbs ("m", mat, mat,
                                     (L'.ECase ((L'.EField ((L'.ERel 0, loc), "1"), loc),
                                                [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                                                  (L'.ERecord [("1", str (Settings.mangleSql (lowercaseFirst nm1)),
                                                                string),
                                                               ("2", str (Settings.mangleSql (lowercaseFirst nm2)),
                                                                string)], loc)),
                                                 ((L'.PVar ("_", string), loc),
                                                  (L'.ERecord [("1", (L'.EStrcat (
                                                                      str (Settings.mangleSql (lowercaseFirst nm1)
                                                                            ^ ", "),
                                                                      (L'.EField ((L'.ERel 1, loc), "1"), loc)),
                                                                      loc), string),
                                                               ("2", (L'.EStrcat (
                                                                      str (Settings.mangleSql (lowercaseFirst nm2)
                                                                           ^ ", "),
                                                                      (L'.EField ((L'.ERel 1, loc), "2"), loc)),
                                                                      loc), string)],
                                                   loc))],
                                                {disc = string,
                                                 result = mat}), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "restrict"), _), _) => (str "RESTRICT", fm)
          | L.ECApp ((L.EFfi ("Basis", "cascade"), _), _) => (str "CASCADE", fm)
          | L.ECApp ((L.EFfi ("Basis", "no_action"), _), _) => (str "NO ACTION", fm)
          | L.ECApp ((L.EFfi ("Basis", "set_null"), _), _) => (str "SET NULL", fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.ECApp (
                  (L.ECApp (
                   (L.EFfi ("Basis", "foreign_key"), _),
                   _), _),
                  _), _),
                 _), _),
                _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val unit = (L'.TRecord [], loc)
                val string = (L'.TFfi ("Basis", "string"), loc)
                val mat = (L'.TRecord [("1", string), ("2", string)], loc)
                val recd = (L'.TRecord [("OnDelete", string),
                                        ("OnUpdate", string)], loc)

                fun strcat [] = raise Fail "Monoize.strcat"
                  | strcat [e] = e
                  | strcat (e1 :: es) = (L'.EStrcat (e1, strcat es), loc)

                fun prop (fd, kw) =
                    (L'.ECase ((L'.EField ((L'.ERel 0, loc), fd), loc),
                               [((L'.PPrim (Prim.String (Prim.Normal, "NO ACTION")), loc),
                                 str ""),
                                ((L'.PVar ("_", string), loc),
                                 strcat [str (" ON " ^ kw ^ " "),
                                         (L'.EField ((L'.ERel 1, loc), fd), loc)])],
                               {disc = string,
                                result = string}), loc)
            in
                ((L'.EAbs ("m", mat, (L'.TFun (string, (L'.TFun (recd, string), loc)), loc),
                           (L'.EAbs ("tab", string, (L'.TFun (recd, string), loc),
                                     (L'.EAbs ("pr", recd, string,
                                               strcat [str "FOREIGN KEY (",
                                                       (L'.EField ((L'.ERel 2, loc), "1"), loc),
                                                       str ") REFERENCES ",
                                                       (L'.ERel 1, loc),
                                                       str " (",
                                                       (L'.EField ((L'.ERel 2, loc), "2"), loc),
                                                       str ")",
                                                       prop ("OnDelete", "DELETE"),
                                                       prop ("OnUpdate", "UPDATE")]), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.ECApp (
                  (L.EFfi ("Basis", "sql_exp_weaken"), _),
                  _), _),
                 _), _),
                _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val string = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("e", string, string, (L'.ERel 0, loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "check"), _), _) =>
            let
                val string = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("e", string, string,
                           (L'.EStrcat (str "CHECK ",
                                        (L'.EFfiApp ("Basis", "checkString",
                                                     [((L'.ERel 0, loc), string)]), loc)), loc)), loc),
                 fm)
            end

          | L.EFfiApp ("Basis", "dml", [(e, _)]) =>
            let
                val string = (L'.TFfi ("Basis", "string"), loc)
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.ECase (e,
                            [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                              (L'.ERecord [], loc)),
                             ((L'.PVar ("cmd", string), loc),
                              (L'.EDml ((L'.ERel 0, loc), L'.Error), loc))],
                            {disc = string,
                             result = (L'.TRecord [], loc)}), loc),
                 fm)
            end

          | L.EFfiApp ("Basis", "tryDml", [(e, _)]) =>
            let
                val string = (L'.TFfi ("Basis", "string"), loc)
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.ECase (e,
                            [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                              (L'.ERecord [], loc)),
                             ((L'.PVar ("cmd", string), loc),
                              (L'.EDml ((L'.ERel 0, loc), L'.None), loc))],
                            {disc = string,
                             result = (L'.TRecord [], loc)}), loc),
                 fm)
            end

          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "insert"), _), fields), _), _) =>
            (case monoType env (L.TRecord fields, loc) of
                 (L'.TRecord fields, _) =>
                 let
                     val s = (L'.TFfi ("Basis", "string"), loc)
                     val fields = map (fn (x, _) => (x, s)) fields
                     val rt = (L'.TRecord fields, loc)
                 in
                     ((L'.EAbs ("tab", s, (L'.TFun (rt, s), loc),
                                (L'.EAbs ("fs", rt, s,
                                          strcat [str "INSERT INTO ",
                                                  (L'.ERel 1, loc),
                                                  str " (",
                                                  strcatComma (map (fn (x, _) => str (Settings.mangleSql x)) fields),
                                                  str ") VALUES (",
                                                  strcatComma (map (fn (x, _) =>
                                                                       (L'.EField ((L'.ERel 0, loc),
                                                                                   x), loc)) fields),
                                                  str ")"]), loc)), loc),
                      fm)
                 end
               | _ => poly ())

          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "update"), _), _), _), _), _), changed) =>
            (case monoType env (L.TRecord changed, loc) of
                 (L'.TRecord [], _)  =>
                 let
                     val s = (L'.TFfi ("Basis", "string"), loc)
                     val rt = (L'.TRecord [], loc)
                 in
                     ((L'.EAbs ("fs", rt, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                (L'.EAbs ("tab", s, (L'.TFun (s, s), loc),
                                          (L'.EAbs ("e", s, s,
                                                    str ""), loc)), loc)), loc),
                      fm)
                 end
               | (L'.TRecord changed, _) =>
                 let
                     val s = (L'.TFfi ("Basis", "string"), loc)
                     val changed = map (fn (x, _) => (x, s)) changed
                     val rt = (L'.TRecord changed, loc)
                 in
                     ((L'.EAbs ("fs", rt, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                (L'.EAbs ("tab", s, (L'.TFun (s, s), loc),
                                          (L'.EAbs ("e", s, s,
                                                    if #supportsUpdateAs (Settings.currentDbms ()) then
                                                        strcat [str "UPDATE ",
                                                                (L'.ERel 1, loc),
                                                                str " AS T_T SET ",
                                                                strcatComma (map (fn (x, _) =>
                                                                                     strcat [str (Settings.mangleSql x
                                                                                                 ^ " = "),
                                                                                             (L'.EField
                                                                                                  ((L'.ERel 2,
                                                                                                    loc),
                                                                                                   x), loc)])
                                                                                 changed),
                                                                str " WHERE ",
                                                                (L'.ERel 0, loc)]
                                                    else
                                                        strcat [str "UPDATE ",
                                                                (L'.ERel 1, loc),
                                                                str " SET ",
                                                                strcatComma (map (fn (x, _) =>
                                                                                     strcat [str (Settings.mangleSql x
                                                                                                 ^ " = "),
                                                                                             (L'.EFfiApp ("Basis", "unAs",
                                                                                                          [((L'.EField
                                                                                                                 ((L'.ERel 2,
                                                                                                                   loc),
                                                                                                                  x), loc),
                                                                                                            s)]), loc)])
                                                                                 changed),
                                                                str " WHERE ",
                                                                (L'.EFfiApp ("Basis", "unAs", [((L'.ERel 0, loc), s)]), loc)]),
                                           loc)), loc)), loc),
                      fm)
                 end
               | _ => poly ())

          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "delete"), _), _), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("tab", s, (L'.TFun (s, s), loc),
                           (L'.EAbs ("e", s, s,
                                     if #supportsDeleteAs (Settings.currentDbms ()) then
                                         strcat [str "DELETE FROM ",
                                                 (L'.ERel 1, loc),
                                                 str " AS T_T WHERE ",
                                                 (L'.ERel 0, loc)]
                                     else
                                         strcat [str "DELETE FROM ",
                                                 (L'.ERel 1, loc),
                                                 str " WHERE ",
                                                 (L'.EFfiApp ("Basis", "unAs", [((L'.ERel 0, loc), s)]), loc)]), loc)), loc),
                 fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp ((L.EFfi ("Basis", "query"), _), (L.CRecord (_, tables), _)), _),
             exps), _),
            state) =>
            (case monoType env (L.TRecord exps, loc) of
                 (L'.TRecord exps, _) =>
                 let
                     val tables = map (fn ((L.CName x, _), xts) =>
                                        (case monoType env (L.TRecord xts, loc) of
                                             (L'.TRecord xts, _) => SOME (x, xts)
                                           | _ => NONE)
                                      | _ => NONE) tables
                 in
                     if List.exists (fn x => x = NONE) tables then
                         poly ()
                     else
                         let
                             val tables = List.mapPartial (fn x => x) tables
                             val state = monoType env state
                             val s = (L'.TFfi ("Basis", "string"), loc)
                             val un = (L'.TRecord [], loc)

                             val rt = exps @ map (fn (x, xts) => (x, (L'.TRecord xts, loc))) tables
                             val ft = (L'.TFun ((L'.TRecord rt, loc),
                                                (L'.TFun (state,
                                                          (L'.TFun (un, state), loc)),
                                                 loc)), loc)

                             val body' = (L'.EApp (
                                          (L'.EApp (
                                           (L'.EApp ((L'.ERel 4, loc),
                                                     (L'.ERel 1, loc)), loc),
                                           (L'.ERel 0, loc)), loc),
                                          (L'.ERecord [], loc)), loc)
                             val body = (L'.EQuery {exps = exps,
                                                    tables = tables,
                                                    state = state,
                                                    query = (L'.ERel 3, loc),
                                                    body = body',
                                                    initial = (L'.ERel 1, loc)},
                                         loc)
                         in
                             ((L'.EAbs ("q", s, (L'.TFun (ft, (L'.TFun (state, (L'.TFun (un, state), loc)), loc)), loc),
                                        (L'.EAbs ("f", ft, (L'.TFun (state, (L'.TFun (un, state), loc)), loc),
                                                  (L'.EAbs ("i", state, (L'.TFun (un, state), loc),
                                                            (L'.EAbs ("_", un, state,
                                                                      body), loc)), loc)), loc)), loc), fm)
                         end
                 end
               | _ => poly ())

          | L.ECApp ((L.ECApp ((L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_query"), _), _), _), _), _), _), _), _), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                fun gf s = (L'.EField ((L'.ERel 0, loc), s), loc)
            in
                ((L'.EAbs ("r",
                           (L'.TRecord [("Rows", s), ("OrderBy", s), ("Limit", s), ("Offset", s)], loc),
                           s,
                           strcat [gf "Rows",
                                   (L'.ECase (gf "OrderBy",
                                              [((L'.PPrim (Prim.String (Prim.Normal, "")), loc), str ""),
                                               ((L'.PVar ("orderby", s), loc),
                                                strcat [str " ORDER BY ",
                                                        (L'.ERel 0, loc)])],
                                              {disc = s, result = s}), loc),
                                   gf "Limit",
                                   gf "Offset"]), loc), fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.ECApp (
                  (L.EFfi ("Basis", "sql_query1"), _),
                  _), _),
                 _), _),
                (L.CRecord (_, tables), _)), _),
               (L.CRecord (_, grouped), _)), _),
              (L.CRecord (_, stables), _)), _),
             sexps), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val b = (L'.TFfi ("Basis", "bool"), loc)
                val un = (L'.TRecord [], loc)
                fun gf s = (L'.EField ((L'.ERel 0, loc), s), loc)

                fun doTables tables =
                    let
                        val tables = map (fn ((L.CName x, _), xts) =>
                                             (case monoType env (L.TRecord xts, loc) of
                                                  (L'.TRecord xts, _) => SOME (x, xts)
                                                | _ => NONE)
                                           | _ => NONE) tables
                    in
                        if List.exists (fn x => x = NONE) tables then
                            NONE
                        else
                            let
                                val tables = List.mapPartial (fn x => x) tables
                                val tables = ListMergeSort.sort
                                                 (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER)
                                                 tables
                                val tables = map (fn (x, xts) =>
                                                     (x, ListMergeSort.sort
                                                             (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER)
                                                             xts)) tables
                            in
                                SOME tables
                            end
                    end
            in
                case (doTables tables, doTables grouped, doTables stables, monoType env (L.TRecord sexps, loc)) of
                    (SOME tables, SOME grouped, SOME stables, (L'.TRecord sexps, _)) =>
                    let
                        val sexps = ListMergeSort.sort
                                        (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) sexps
                    in
                        ((L'.EAbs ("r",
                                   (L'.TRecord [("Distinct", b),
                                                ("From", s),
                                                ("Where", s),
                                                ("GroupBy", un),
                                                ("Having", s),
                                                ("SelectFields", un),
                                                ("SelectExps", (L'.TRecord (map (fn (x, _) => (x, s)) sexps), loc))],
                                    loc),
                                   s,
                                   strcat [str "SELECT ",
                                           (L'.ECase (gf "Distinct",
                                                      [((L'.PCon (L'.Enum,
                                                                  L'.PConFfi {mod = "Basis",
                                                                              datatyp = "bool",
                                                                              con = "True",
                                                                              arg = NONE},
                                                                  NONE), loc),
                                                        str "DISTINCT "),
                                                       ((L'.PCon (L'.Enum,
                                                                  L'.PConFfi {mod = "Basis",
                                                                              datatyp = "bool",
                                                                              con = "False",
                                                                              arg = NONE},
                                                                  NONE), loc),
                                                        str "")],
                                                      {disc = b, result = s}), loc),
                                           if List.null sexps andalso List.all (List.null o #2) stables then
                                               str "0"
                                           else
                                               strcatComma (map (fn (x, t) =>
                                                                    strcat [
                                                                        (L'.EField (gf "SelectExps", x), loc),
                                                                        str (" AS " ^ Settings.mangleSql x)
                                                                ]) sexps
                                                            @ map (fn (x, xts) =>
                                                                      strcatComma
                                                                          (map (fn (x', _) =>
                                                                                   str ("T_" ^ x
										        ^ "."
										        ^ Settings.mangleSql x'))
                                                                               xts)) stables),
                                           (L'.ECase (gf "From",
                                                      [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                                                        str ""),
                                                       ((L'.PVar ("x", s), loc),
                                                        strcat [str " FROM ",
                                                                (L'.ERel 0, loc)])],
                                                      {disc = s,
                                                       result = s}), loc),
                                           (L'.ECase (gf "Where",
                                                      [((L'.PPrim (Prim.String (Prim.Normal, #trueString (Settings.currentDbms ()))),
                                                         loc),
                                                        str ""),
                                                       ((L'.PVar ("where", s), loc),
                                                        strcat [str " WHERE ", (L'.ERel 0, loc)])],
                                                      {disc = s,
                                                       result = s}), loc),

                                           if List.all (fn (x, xts) =>
                                                           case List.find (fn (x', _) => x' = x) grouped of
                                                               NONE => List.null xts
                                                             | SOME (_, xts') =>
                                                               List.all (fn (x, _) =>
                                                                            List.exists (fn (x', _) => x' = x)
                                                                                        xts') xts) tables then
                                               str ""
                                           else
                                               strcat [
                                               str " GROUP BY ",
                                               strcatComma (map (fn (x, xts) =>
                                                                    strcatComma
                                                                        (map (fn (x', _) =>
                                                                                 str ("T_" ^ x
										     ^ "."
										     ^ Settings.mangleSql x'))
                                                                             xts)) grouped)
                                               ],

                                           (L'.ECase (gf "Having",
                                                      [((L'.PPrim (Prim.String
                                                                       (Prim.Normal, #trueString (Settings.currentDbms ()))), loc),
                                                        str ""),
                                                       ((L'.PVar ("having", s), loc),
                                                        strcat [str " HAVING ", (L'.ERel 0, loc)])],
                                                      {disc = s,
                                                       result = s}), loc)
                                  ]), loc),
                         fm)
                    end
                  | _ => poly ()
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_inject"), _),
               _), _),
              _), _),
             _), _),
            t) =>
            let
                val t = monoType env t
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("f", (L'.TFun (t, s), loc), (L'.TFun (t, s), loc),
                           (L'.ERel 0, loc)), loc), fm)
            end

          | L.EFfi ("Basis", "sql_int") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "int"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyInt", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "int"), loc))]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_float") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyFloat", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "float"), loc))]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_bool") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "bool"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyBool", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "bool"), loc))]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_string") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "string"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyString", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "string"), loc))]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_char") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "char"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyChar", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "char"), loc))]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_time") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "time"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyTime", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "time"), loc))]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_blob") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "blob"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyBlob", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "blob"), loc))]), loc)), loc),
             fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_channel"), _), _) =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "channel"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyChannel", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "channel"), loc))]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_client") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "client"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyClient", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "client"), loc))]), loc)), loc),
             fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_serialized"), _), _) =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "string"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyString", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "string"), loc))]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_url") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "string"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyString", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "string"), loc))]), loc)), loc),
             fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_prim"), _), t) =>
            let
                val t = monoType env t
                val tf = (L'.TFun (t, (L'.TFfi ("Basis", "string"), loc)), loc)
            in
                ((L'.EAbs ("f", tf, tf, (L'.ERel 0, loc)), loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "sql_option_prim"), _), t) =>
            let
                val t = monoType env t
                val s = (L'.TFfi ("Basis", "string"), loc)

                fun toSqlType (t : L'.typ) =
                    case #1 t of
                        L'.TFfi ("Basis", "int") => Settings.Int
                      | L'.TFfi ("Basis", "float") => Settings.Float
                      | L'.TFfi ("Basis", "string") => Settings.String
                      | L'.TFfi ("Basis", "char") => Settings.Char
                      | L'.TFfi ("Basis", "bool") => Settings.Bool
                      | L'.TFfi ("Basis", "time") => Settings.Time
                      | L'.TFfi ("Basis", "blob") => Settings.Blob
                      | L'.TFfi ("Basis", "channel") => Settings.Channel
                      | L'.TFfi ("Basis", "client") => Settings.Client
                      | _ => raise Fail "Monoize/sql_option_prim: invalid SQL type"
            in
                ((L'.EAbs ("f",
                           (L'.TFun (t, s), loc),
                           (L'.TFun ((L'.TOption t, loc), s), loc),
                           (L'.EAbs ("x",
                                     (L'.TOption t, loc),
                                     s,
                                     (L'.ECase ((L'.ERel 0, loc),
                                                [((L'.PNone t, loc),
                                                  str (#p_cast (Settings.currentDbms ()) ("NULL", toSqlType t))),
                                                 ((L'.PSome (t, (L'.PVar ("y", t), loc)), loc),
                                                  (L'.EApp ((L'.ERel 2, loc), (L'.ERel 0, loc)), loc))],
                                                {disc = (L'.TOption t, loc),
                                                 result = s}), loc)), loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "trigrammable_string") =>
            ((L'.ERecord [], loc),
             fm)
          | L.EFfi ("Basis", "trigrammable_option_string") =>
            ((L'.ERecord [], loc),
             fm)

          | L.ECApp ((L.EFfi ("Basis", "nullify_option"), _), _) =>
            ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "nullify_prim"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TRecord [], loc),
                       (L'.ERecord [], loc)), loc),
             fm)

          | L.ECApp ((L.EFfi ("Basis", "sql_subset"), _), _) =>
            ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_subset_all"), _), _) =>
            ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_subset_concat"),
                                                    _), _), _), _), _), _), _), _) =>
            let
                val un = (L'.TRecord [], loc)
            in
                ((L'.EAbs ("_", un, (L'.TFun (un, un), loc),
                           (L'.EAbs ("_", un, un,
                                     (L'.ERecord [], loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "fieldsOf_table"), _), _), _), _) =>
            ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "fieldsOf_view"), _), _) =>
            ((L'.ERecord [], loc), fm)

          | L.ECApp ((L.EFfi ("Basis", "sql_from_nil"), _), _) =>
            (str "", fm)
          | L.ECApp ((L.EApp ((L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_from_table"), _), _),
                                                   _), _), _), _), _), _), _),
                     (L.CName name, _)) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("tab", s, s,
                           strcat [(L'.ERel 0, loc),
                                   str (" AS T_" ^ name)]), loc),
                 fm)
            end
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_from_query"), _), _),
                                          _), _), _),
                     (L.CName name, _)) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("q", s, s,
                           strcat [str "(",
                                   (L'.ERel 0, loc),
                                   str (") AS T_" ^ name)]), loc),
                 fm)
            end
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_from_comma"), _), _), _), _), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val disc = (L'.TRecord [("1", s), ("2", s)], loc)
            in
                ((L'.EAbs ("tab1", s, (L'.TFun (s, s), loc),
                           (L'.EAbs ("tab2", s, s,
                                     (L'.ECase ((L'.ERecord [("1", (L'.ERel 1, loc), s),
                                                             ("2", (L'.ERel 0, loc), s)], loc),
                                                [((L'.PRecord [("1", (L'.PPrim (Prim.String (Prim.Normal, "")), loc), s)], loc),
                                                  (L'.ERel 0, loc)),
                                                 ((L'.PRecord [("2", (L'.PPrim (Prim.String (Prim.Normal, "")), loc), s)], loc),
                                                  (L'.ERel 1, loc)),
                                                 ((L'.PVar ("_", disc), loc),
                                                  strcat [(L'.ERel 2, loc),
                                                          str ", ",
                                                          (L'.ERel 1, loc)])],
                                                {disc = disc,
                                                 result = s}), loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_inner_join"), _), _), _), _), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val disc = (L'.TRecord [("1", s), ("2", s)], loc)
            in
                ((L'.EAbs ("tab1", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("tab2", s, (L'.TFun (s, s), loc),
                                     (L'.EAbs ("on", s, s,
                                               (L'.ECase ((L'.ERecord [("1", (L'.ERel 2, loc), s),
                                                                       ("2", (L'.ERel 1, loc), s)], loc),
                                                          [((L'.PRecord [("1", (L'.PPrim (Prim.String (Prim.Normal, "")), loc), s)], loc),
                                                            (L'.ERel 1, loc)),
                                                           ((L'.PRecord [("2", (L'.PPrim (Prim.String (Prim.Normal, "")), loc), s)], loc),
                                                            (L'.ERel 2, loc)),
                                                           ((L'.PVar ("_", disc), loc),
                                                            strcat [(L'.ERel 3, loc),
                                                                    str " JOIN ",
                                                                    (L'.ERel 2, loc),
                                                                    str " ON ",
                                                                    (L'.ERel 1, loc)])],
                                                          {disc = disc,
                                                           result = s}), loc)), loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_left_join"), _), _), _), _), _),
                     (L.CRecord (_, right), _)) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val disc = (L'.TRecord [("1", s), ("2", s)], loc)
            in
                ((L'.EAbs ("_", outerRec right,
                           (L'.TFun (s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc)), loc),
                           (L'.EAbs ("tab1", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                     (L'.EAbs ("tab2", s, (L'.TFun (s, s), loc),
                                               (L'.EAbs ("on", s, s,
                                                         (L'.ECase ((L'.ERecord [("1", (L'.ERel 2, loc), s),
                                                                                 ("2", (L'.ERel 1, loc), s)], loc),
                                                                    [((L'.PRecord [("1", (L'.PPrim (Prim.String (Prim.Normal, "")),
                                                                                          loc), s)], loc),
                                                                      (L'.ERel 1, loc)),
                                                                     ((L'.PRecord [("2", (L'.PPrim (Prim.String (Prim.Normal, "")),
                                                                                          loc), s)], loc),
                                                                      (L'.ERel 2, loc)),
                                                                     ((L'.PVar ("_", disc), loc),
                                                                      strcat [(L'.ERel 3, loc),
                                                                              str " LEFT JOIN ",
                                                                              (L'.ERel 2, loc),
                                                                              str " ON ",
                                                                              (L'.ERel 1, loc)])],
                                                                    {disc = disc,
                                                                     result = s}), loc)), loc)), loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_right_join"), _), (L.CRecord (_, left), _)),
                                _), _), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val disc = (L'.TRecord [("1", s), ("2", s)], loc)
            in
                ((L'.EAbs ("_", outerRec left,
                           (L'.TFun (s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc)), loc),
                           (L'.EAbs ("tab1", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                     (L'.EAbs ("tab2", s, (L'.TFun (s, s), loc),
                                               (L'.EAbs ("on", s, s,
                                                         (L'.ECase ((L'.ERecord [("1", (L'.ERel 2, loc), s),
                                                                                 ("2", (L'.ERel 1, loc), s)], loc),
                                                                    [((L'.PRecord [("1", (L'.PPrim (Prim.String (Prim.Normal, "")),
                                                                                          loc), s)], loc),
                                                                      (L'.ERel 1, loc)),
                                                                     ((L'.PRecord [("2", (L'.PPrim (Prim.String (Prim.Normal, "")),
                                                                                          loc), s)], loc),
                                                                      (L'.ERel 2, loc)),
                                                                     ((L'.PVar ("_", disc), loc),
                                                                      strcat [(L'.ERel 3, loc),
                                                                              str " RIGHT JOIN ",
                                                                              (L'.ERel 2, loc),
                                                                              str " ON ",
                                                                              (L'.ERel 1, loc)])],
                                                                    {disc = disc,
                                                                     result = s}), loc)), loc)), loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_full_join"), _), (L.CRecord (_, left), _)), _),
                     (L.CRecord (_, right), _)), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val disc = (L'.TRecord [("1", s), ("2", s)], loc)
            in
                ((L'.EAbs ("_", outerRec (left @ right),
                           (L'.TFun (s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc)), loc),
                           (L'.EAbs ("tab1", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                     (L'.EAbs ("tab2", s, (L'.TFun (s, s), loc),
                                               (L'.EAbs ("on", s, s,
                                                         (L'.ECase ((L'.ERecord [("1", (L'.ERel 2, loc), s),
                                                                                 ("2", (L'.ERel 1, loc), s)], loc),
                                                                    [((L'.PRecord [("1", (L'.PPrim (Prim.String (Prim.Normal, "")),
                                                                                          loc), s)], loc),
                                                                      (L'.ERel 1, loc)),
                                                                     ((L'.PRecord [("2", (L'.PPrim (Prim.String (Prim.Normal, "")),
                                                                                          loc), s)], loc),
                                                                      (L'.ERel 2, loc)),
                                                                     ((L'.PVar ("_", disc), loc),
                                                                      strcat [(L'.ERel 3, loc),
                                                                              str " FULL JOIN ",
                                                                              (L'.ERel 2, loc),
                                                                              str " ON ",
                                                                              (L'.ERel 1, loc)])],
                                                                    {disc = disc,
                                                                     result = s}), loc)), loc)), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_order_by_Nil"), _), _), _), _) =>
            (str "", fm)
          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_order_by_random"), _), _), _), _) =>
            (str (#randomFunction (Settings.currentDbms ()) ^ "()"), fm)
          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_order_by_Cons"), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFun (s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc)), loc),
                           (L'.EAbs ("e1", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                     (L'.EAbs ("d", s, (L'.TFun (s, s), loc),
                                               (L'.EAbs ("e2", s, s,
                                                         (L'.ECase ((L'.ERel 0, loc),
                                                                    [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                                                                      strcat [(L'.ERel 2, loc),
                                                                              (L'.ERel 1, loc)]),
                                                                     ((L'.PVar ("_", s), loc),
                                                                      strcat [(L'.ERel 3, loc),
                                                                              (L'.ERel 2, loc),
                                                                              str ", ",
                                                                              (L'.ERel 1, loc)])],
                                                                    {disc = s, result = s}), loc)), loc)), loc)), loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "sql_no_limit") =>
            (str "", fm)
          | L.EFfiApp ("Basis", "sql_limit", [(e, t)]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                (strcat [
                 str " LIMIT ",
                 (L'.EFfiApp ("Basis", "sqlifyInt", [(e, monoType env t)]), loc)
                 ],
                 fm)
            end

          | L.EFfi ("Basis", "sql_no_offset") =>
            (str "", fm)
          | L.EFfiApp ("Basis", "sql_offset", [(e, t)]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                (strcat [
                 str " OFFSET ",
                 (L'.EFfiApp ("Basis", "sqlifyInt", [(e, monoType env t)]), loc)
                 ],
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "sql_eq"), _), _) =>
            (str "=", fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_ne"), _), _) =>
            (str "<>", fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_lt"), _), _) =>
            (str "<", fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_le"), _), _) =>
            (str "<=", fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_gt"), _), _) =>
            (str ">", fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_ge"), _), _) =>
            (str ">=", fm)

          | L.ECApp ((L.EFfi ("Basis", "sql_plus"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       str "+"), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_minus"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       str "-"), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_times"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       str "*"), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_div"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       str "/"), loc), fm)
          | L.EFfi ("Basis", "sql_mod") =>
            (str "%", fm)
          | L.EFfi ("Basis", "sql_concat") =>
            (str "||", fm)

          | L.EFfi ("Basis", "sql_like") =>
            (str "LIKE", fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_distance"), _), _) =>
            ((case #supportsSimilar (Settings.currentDbms ()) of
                  NONE => ErrorMsg.errorAt loc "The DBMS you've selected doesn't support <->."
                | _ => ());
             uses_similar := true;
             (((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                         str "<->"), loc), fm)))

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                 (L.EFfi ("Basis", "sql_unary"), _),
                 _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("c", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("e1", s, (L'.TFun (s, s), loc),
                                     strcat [str "(",
                                             (L'.ERel 1, loc),
                                             str " ",
                                             (L'.ERel 0, loc),
                                             str ")"]), loc)), loc),
                 fm)
            end
          | L.EFfi ("Basis", "sql_not") => (str "NOT", fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_neg"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       str "-"), loc), fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.EFfi ("Basis", "sql_binary"), _),
                 _), _),
                _), _),
               _), _),
              arg1), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)

                fun default n = strcat [str "(",
                                        (L'.ERel (n + 1), loc),
                                        str " ",
                                        (L'.ERel (n + 2), loc),
                                        str " ",
                                        (L'.ERel n, loc),
                                        str ")"]

                val body = case #1 arg1 of
                               L.CApp ((L.CFfi ("Basis", "option"), _), _) =>
                               (L'.ECase ((L'.ERel 2, loc),
                                          [((L'.PPrim (Prim.String (Prim.Normal, "=")), loc),
                                            if #supportsIsDistinctFrom (Settings.currentDbms ()) then
                                                strcat [str "((",
                                                        (L'.ERel 1, loc),
                                                        str " IS NOT DISTINCT FROM ",
                                                        (L'.ERel 0, loc),
                                                        str "))"]
                                            else
                                                strcat [str "((",
                                                        (L'.ERel 1, loc),
                                                        str " ",
                                                        (L'.ERel 2, loc),
                                                        str " ",
                                                        (L'.ERel 0, loc),
                                                        str ") OR ((",
                                                        (L'.ERel 1, loc),
                                                        str ") IS NULL AND (",
                                                        (L'.ERel 0, loc),
                                                        str ") IS NULL))"]),
                                           ((L'.PVar ("_", s), loc),
                                            default 1)],
                                          {disc = s,
                                           result = s}), loc)
                               | _ => default 0
            in
                ((L'.EAbs ("c", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("e1", s, (L'.TFun (s, s), loc),
                                     (L'.EAbs ("e2", s, s,
                                               body), loc)), loc)), loc),
                 fm)
            end
          | L.EFfi ("Basis", "sql_and") => (str "AND", fm)
          | L.EFfi ("Basis", "sql_or") => (str "OR", fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.ECApp (
                  (L.EFfi ("Basis", "sql_field"), _),
                  _), _),
                 _), _),
                _), _),
               _), _),
              _), _),
             (L.CName tab, _)), _),
            (L.CName field, _)) => (str ("T_" ^ tab ^ "." ^ Settings.mangleSql (lowercaseFirst field)), fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_exp"), _),
                _), _),
               _), _),
              _), _),
             _), _),
            (L.CName nm, _)) => (str (Settings.mangleSql (lowercaseFirst nm)), fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.EFfi ("Basis", "sql_relop"), _),
                 _), _),
                _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val disc = (L'.TFfi ("Basis", "bool"), loc)
            in
                (if #nestedRelops (Settings.currentDbms ()) then
                     (L'.EAbs ("c", s, (L'.TFun ((L'.TFfi ("Basis", "bool"), loc), (L'.TFun (s, (L'.TFun (s, s), loc)), loc)), loc),
                               (L'.EAbs ("all", (L'.TFfi ("Basis", "bool"), loc), (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                         (L'.EAbs ("e1", s, (L'.TFun (s, s), loc),
                                                   (L'.EAbs ("e2", s, s,
                                                             strcat [str "((",
                                                                     (L'.ERel 1, loc),
                                                                     str ") ",
                                                                     (L'.ERel 3, loc),
                                                                     (L'.ECase ((L'.ERel 2, loc),
                                                                                [((L'.PCon (L'.Enum, L'.PConFfi {mod = "Basis",
                                                                                                                 datatyp = "bool",
                                                                                                                 con = "True",
                                                                                                                 arg = NONE}, NONE), loc),
                                                                                  str " ALL"),
                                                                                 ((L'.PVar ("_", disc), loc),
                                                                                  str "")],
                                                                                {disc = disc,
                                                                                 result = s}), loc),
                                                                     str " (",
                                                                     (L'.ERel 0, loc),
                                                                     str "))"]), loc)), loc)), loc)), loc)
                 else
                     (L'.EAbs ("c", s, (L'.TFun ((L'.TFfi ("Basis", "bool"), loc), (L'.TFun (s, (L'.TFun (s, s), loc)), loc)), loc),
                               (L'.EAbs ("all", (L'.TFfi ("Basis", "bool"), loc), (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                         (L'.EAbs ("e1", s, (L'.TFun (s, s), loc),
                                                   (L'.EAbs ("e2", s, s,
                                                             strcat [(L'.ERel 1, loc),
                                                                     str " ",
                                                                     (L'.ERel 3, loc),
                                                                     (L'.ECase ((L'.ERel 2, loc),
                                                                                [((L'.PCon (L'.Enum, L'.PConFfi {mod = "Basis",
                                                                                                                 datatyp = "bool",
                                                                                                                 con = "True",
                                                                                                                 arg = NONE}, NONE), loc),
                                                                                  str " ALL"),
                                                                                 ((L'.PVar ("_", disc), loc),
                                                                                  str "")],
                                                                                {disc = disc,
                                                                                 result = s}), loc),
                                                                     str " ",
                                                                     (L'.ERel 0, loc)]), loc)), loc)), loc)), loc),
                 fm)
            end
          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_forget_tables"), _),
                _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("x", s, s, (L'.ERel 0, loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "sql_union") => (str "UNION", fm)
          | L.EFfi ("Basis", "sql_intersect") =>
            (if #onlyUnion (Settings.currentDbms ()) then
                 ErrorMsg.errorAt loc "The DBMS you've selected doesn't support INTERSECT."
             else
                 ();
             (str "INTERSECT", fm))
          | L.EFfi ("Basis", "sql_except") =>
            (if #onlyUnion (Settings.currentDbms ()) then
                 ErrorMsg.errorAt loc "The DBMS you've selected doesn't support EXCEPT."
             else
                 ();
             (str "EXCEPT", fm))

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.EFfi ("Basis", "sql_count"), _),
              _), _),
             _), _),
            _) => (str "COUNT(*)", fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_aggregate"), _),
                _), _),
               _), _),
              _), _),
             _), _),
            t) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)

                val main = strcat [(L'.ERel 1, loc),
                                   str "(",
                                   (L'.ERel 0, loc),
                                   str ")"]
            in
                ((L'.EAbs ("c", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("e1", s, (L'.TFun (s, s), loc), main), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "sql_count_col"), _), _) =>
            (str "COUNT", fm)

          | L.EFfi ("Basis", "sql_summable_int") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_summable_float") => ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_summable_option"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TRecord [], loc),
                       (L'.ERecord [], loc)), loc),
             fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_avg"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       str "AVG"), loc),
             fm)
          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_sum"), _), _), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFun ((L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc)), loc),
                       (L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                                 str "SUM"), loc)), loc),
             fm)

          | L.EFfi ("Basis", "sql_arith_int") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_arith_float") => ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_arith_option"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TRecord [], loc),
                       (L'.ERecord [], loc)), loc),
             fm)

          | L.EFfi ("Basis", "sql_maxable_int") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_maxable_float") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_maxable_string") => ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_maxable_option"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TRecord [], loc),
                       (L'.ERecord [], loc)), loc),
             fm)
          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_max"), _), _), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFun ((L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc)), loc),
                       (L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                                 str "MAX"), loc)), loc),
             fm)
          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_min"), _), _), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFun ((L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc)), loc),
                       (L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                                 str "MIN"), loc)), loc),
             fm)

          | L.EFfi ("Basis", "sql_asc") => (str "", fm)
          | L.EFfi ("Basis", "sql_desc") => (str " DESC", fm)
          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_nfunc"), _),
               _), _),
              _), _),
             _), _),
           _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "sql_window_normal") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_window_fancy") => ((L'.ERecord [], loc), fm)
          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_window"), _),
                _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFun (s, s), loc),
                           (L'.EAbs ("e", s, s,
                                     (L'.ERel 0, loc)), loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "sql_current_timestamp") => (str "CURRENT_TIMESTAMP", fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_ufunc"), _),
                _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("f", s, (L'.TFun (s, s), loc),
                           (L'.EAbs ("x", s, s,
                                     strcat [(L'.ERel 1, loc),
                                             str "(",
                                             (L'.ERel 0, loc),
                                             str ")"]), loc)), loc),
                 fm)
            end
          | L.EFfi ("Basis", "sql_octet_length") =>
            (str (if #supportsOctetLength (Settings.currentDbms ()) then
                      "octet_length"
                  else
                      "length"), fm)
          | L.EFfi ("Basis", "sql_lower") =>
            (str "lower", fm)
          | L.EFfi ("Basis", "sql_upper") =>
            (str "upper", fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_known"), _), _) =>
            ((L'.EFfi ("Basis", "sql_known"), loc), fm)

          | L.ECApp (
                (L.ECApp (
                      (L.ECApp (
                            (L.ECApp (
                                  (L.ECApp (
                                        (L.ECApp (
                                              (L.EFfi ("Basis", "sql_bfunc"), _),
                                              _), _),
                                        _), _),
                                  _), _),
                            _), _),
                      _), _),
                _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("f", s, (L'.TFun (s, s), loc),
                           (L'.EAbs ("x1", s, s,
                                     (L'.EAbs ("x2", s, s,
                                               strcat [(L'.ERel 2, loc),
                                                       str "(",
                                                       (L'.ERel 1, loc),
                                                       str ",",
                                                       (L'.ERel 0, loc),
                                                       str ")"]), loc)), loc)), loc),
                 fm)
            end
          | (L.ECApp (
                  (L.EFfi ("Basis", "sql_similarity"), _),
                  _)) =>
            ((case #supportsSimilar (Settings.currentDbms ()) of
                  NONE => ErrorMsg.errorAt loc "The DBMS you've selected doesn't support SIMILAR."
                | _ => ());
             uses_similar := true;
             ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                        str "similarity"), loc),
              fm))

          | (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_is_null"), _), _),
                _), _),
               _), _),
              _), _)) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("s", s, s,
                           strcat [str "(",
                                   (L'.ERel 0, loc),
                                   str " IS NULL)"]), loc),
                 fm)
            end

          | (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_coalesce"), _), _),
                _), _),
               _), _),
              _), _)) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("x1", s, (L'.TFun (s, s), loc),
                           (L'.EAbs ("x1", s, s,
                                     strcat [str "COALESCE(",
                                             (L'.ERel 1, loc),
                                             str ",",
                                             (L'.ERel 0, loc),
                                             str ")"]), loc)), loc),
                 fm)
            end

          | (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_if_then_else"), _), _),
                _), _),
               _), _),
              _), _)) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("if", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("then", s, (L'.TFun (s, s), loc),
                                     (L'.EAbs ("else", s, s,
                                               strcat [str "(CASE WHEN (",
                                                       (L'.ERel 2, loc),
                                                       str ") THEN (",
                                                       (L'.ERel 1, loc),
                                                       str ") ELSE (",
                                                       (L'.ERel 0, loc),
                                                       str ") END)"]), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_nullable"), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("u", (L'.TRecord [], loc), (L'.TFun (s, s), loc),
                           (L'.EAbs ("x", s, s,
                                     (L'.ERel 0, loc)), loc)), loc),
                 fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.EFfi ("Basis", "sql_subquery"), _),
                 _), _),
                _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFun (s, s), loc),
                           (L'.EAbs ("x", s, s,
                                     strcat [str "(",
                                             (L'.ERel 0, loc),
                                             str ")"]), loc)), loc),
                 fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
               (L.EFfi ("Basis", "sql_no_partition"), _),
               _), _),
             _), _),
            _) => (str "", fm)
          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_partition"), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("e", s, s, strcat [str "PARTITION BY ", (L'.ERel 0, loc)]), loc),
                 fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_window_function"), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val () = if #windowFunctions (Settings.currentDbms ()) then
                             ()
                         else
                             ErrorMsg.errorAt loc "The DBMS you've selected doesn't support window functions."

                val s = (L'.TFfi ("Basis", "string"), loc)

                val main = strcat [(L'.ERel 2, loc),
                                   str " OVER (",
                                   (L'.ERel 1, loc),
                                   (L'.ECase ((L'.ERel 0, loc),
                                              [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                                                str ""),
                                               ((L'.PVar ("_", s), loc),
                                                strcat [str " ORDER BY ",
                                                        (L'.ERel 1, loc)])],
                                              {disc = s,
                                               result = s}), loc),
                                   str ")"]
            in
                ((L'.EAbs ("w", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("p", s, (L'.TFun (s, s), loc),
                                     (L'.EAbs ("o", s, s,
                                               main), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "sql_window_aggregate"), _),
                _), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)

                val main = strcat [(L'.ERel 1, loc),
                                   str "(",
                                   (L'.ERel 0, loc),
                                   str ")"]
            in
                ((L'.EAbs ("c", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("e1", s, s, main), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_window_count"), _), _), _), _), _), _) =>
            (str "COUNT(*)", fm)
          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_rank"), _), _), _), _), _), _) =>
            (str "RANK()", fm)

          | L.EFfiApp ("Basis", "nextval", [(e, _)]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.ENextval e, loc), fm)
            end
          | L.EFfiApp ("Basis", "setval", [(e1, _), (e2, _)]) =>
            let
                val (e1, fm) = monoExp (env, st, fm) e1
                val (e2, fm) = monoExp (env, st, fm) e2
            in
                ((L'.ESetval (e1, e2), loc), fm)
            end

          | L.EFfi ("Basis", "null") => (str "", fm)

          | L.EFfiApp ("Basis", "classes", [(s1, _), (s2, _)]) =>
            let
                val (s1, fm) = monoExp (env, st, fm) s1
                val (s2, fm) = monoExp (env, st, fm) s2
            in
                ((L'.EStrcat (s1, (L'.EStrcat (str " ", s2), loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "data_kind") => (str "data-", fm)
          | L.EFfi ("Basis", "aria_kind") => (str "aria-", fm)

          | L.EFfiApp ("Basis", "data_attr", [(sk, _), (s1, _), (s2, _)]) =>
            let
                val (sk, fm) = monoExp (env, st, fm) sk
                val (s1, fm) = monoExp (env, st, fm) s1
                val (s2, fm) = monoExp (env, st, fm) s2
            in
                ((L'.EStrcat (sk,
                              (L'.EStrcat ((L'.EFfiApp ("Basis", "blessData", [(s1, (L'.TFfi ("Basis", "string"), loc))]), loc),
                                           (L'.EStrcat (str "=\"",
                                                        (L'.EStrcat ((L'.EFfiApp ("Basis", "attrifyString", [(s2, (L'.TFfi ("Basis", "string"), loc))]), loc),
                                                                     str "\""), loc)),
                                            loc)), loc)), loc),
                 fm)
            end

          | L.EFfiApp ("Basis", "data_attrs", [(s1, _), (s2, _)]) =>
            let
                val (s1, fm) = monoExp (env, st, fm) s1
                val (s2, fm) = monoExp (env, st, fm) s2
            in
                ((L'.EStrcat (s1, (L'.EStrcat (str " ", s2), loc)), loc),
                 fm)
            end

          | L.EFfiApp ("Basis", "css_url", [(s, _)]) =>
            let
                val (s, fm) = monoExp (env, st, fm) s
            in
                ((L'.EStrcat (str "url(",
                              (L'.EStrcat ((L'.EFfiApp ("Basis", "css_url", [(s, (L'.TFfi ("Basis", "string"), loc))]), loc),
                                           str ")"), loc)), loc),
                 fm)
            end

          | L.EFfiApp ("Basis", "property", [(s, _)]) =>
            let
                val (s, fm) = monoExp (env, st, fm) s
            in
                ((L'.EStrcat ((L'.EFfiApp ("Basis", "property", [(s, (L'.TFfi ("Basis", "string"), loc))]), loc),
                              str ":"), loc),
                 fm)
            end
          | L.EFfiApp ("Basis", "value", [(s1, _), (s2, _)]) =>
            let
                val (s1, fm) = monoExp (env, st, fm) s1
                val (s2, fm) = monoExp (env, st, fm) s2
            in
                ((L'.EStrcat (s1, (L'.EStrcat (str " ", s2), loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "noStyle") => (str "", fm)
          | L.EFfiApp ("Basis", "oneProperty", [(s1, _), (s2, _)]) =>
            let
                val (s1, fm) = monoExp (env, st, fm) s1
                val (s2, fm) = monoExp (env, st, fm) s2
            in
                ((L'.EStrcat (s1, (L'.EStrcat (s2, str ";"), loc)), loc),
                 fm)
            end

          | L.EApp (
            (L.ECApp (
             (L.ECApp ((L.EFfi ("Basis", "cdata"), _), _), _),
             _), _),
            se) =>
            let
                val (se, fm) = monoExp (env, st, fm) se
            in
                ((L'.EFfiApp ("Basis", "htmlifyString", [(se, (L'.TFfi ("Basis", "string"), loc))]), loc), fm)
            end
          | L.ECApp (
             (L.ECApp ((L.EFfi ("Basis", "cdataChar"), _), _), _),
             _) =>
            ((L'.EAbs ("ch", (L'.TFfi ("Basis", "char"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "htmlifySpecialChar", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "char"), loc))]), loc)), loc), fm)

          | L.EApp (
            (L.EApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.EFfi ("Basis", "join"),
                     _), _), _),
                _), _),
               _), _),
              _), _),
             xml1), _),
            xml2) =>
            let
                val (xml1, fm) = monoExp (env, st, fm) xml1
                val (xml2, fm) = monoExp (env, st, fm) xml2
            in
                ((L'.EStrcat (xml1, xml2), loc), fm)
            end

          | L.EApp (
            (L.EApp (
             (L.EApp (
              (L.EApp (
               (L.EApp (
                (L.EApp (
                 (L.EApp (
		  (L.ECApp (
                   (L.ECApp (
                    (L.ECApp (
                     (L.ECApp (
                      (L.ECApp (
                       (L.ECApp (
                        (L.ECApp (
                         (L.ECApp (
			  (L.EFfi ("Basis", "tag"),
                           _), (L.CRecord (_, attrsGiven), _)), _), _), _), ctxOuter), _), _), _), _), _), _), _), _), _), _), _),
		  class), _),
	         dynClass), _),
                style), _),
               dynStyle), _),
              attrs), _),
             tag), _),
            xml) =>
            let
                fun getTag' (e, _) =
                    case e of
                        L.EFfi (_, tag) => (tag, [])
                      | L.ECApp (e, t) => let
                            val (tag, ts) = getTag' e
                        in
                            (tag, ts @ [t])
                        end
                      | _ => (E.errorAt loc "Non-constant XML tag";
                              Print.eprefaces' [("Expression", CorePrint.p_exp env tag)];
                              ("", []))

                fun getTag (e, _) =
                    case e of
                        L.EFfiApp (_, tag, [((L.ERecord [], _), _)]) => (tag, [])
                      | L.EApp (e, (L.ERecord [], _)) => getTag' e
                      | _ => (E.errorAt loc "Non-constant XML tag";
                              Print.eprefaces' [("Expression", CorePrint.p_exp env tag)];
                              ("", []))

                val (tag, targs) = getTag tag

                val (attrs, fm) = monoExp (env, st, fm) attrs
                val attrs = case #1 attrs of
                                L'.ERecord xes => xes
                              | _ => map (fn ((L.CName x, _), t) => (x, (L'.EField (attrs, x), loc), monoType env t)
                                           | (c, t) => (E.errorAt loc "Non-constant field name for HTML tag attribute";
                                                        Print.eprefaces' [("Name", CorePrint.p_con env c)];
                                                        ("", (L'.EField (attrs, ""), loc), monoType env t))) attrsGiven

                val attrs =
                    if List.exists (fn ("Link", _, _) => true
                                     | _ => false) attrs then
                        List.filter (fn ("Href", _, _) => false
                                      | _ => true) attrs
                    else
                        attrs

                fun findOnload (attrs, onload, onunload, acc) =
                    case attrs of
                        [] => (onload, onunload, acc)
                      | ("Onload", e, _) :: rest => findOnload (rest, SOME e, onunload, acc)
                      | ("Onunload", e, _) :: rest => findOnload (rest, onload, SOME e, acc)
                      | x :: rest => findOnload (rest, onload, onunload, x :: acc)

                val (onload, onunload, attrs) =
                    if tag = "body" then
                        findOnload (attrs, NONE, NONE, [])
                    else
                        (NONE, NONE, attrs)

                val (class, fm) = monoExp (env, st, fm) class
                val (dynClass, fm) = monoExp (env, st, fm) dynClass
                val (style, fm) = monoExp (env, st, fm) style
                val (dynStyle, fm) = monoExp (env, st, fm) dynStyle

                (* Special case for <button value=""> *)
                val (attrs, extraString) = case tag of
                                               "button" =>
                                               (case List.partition (fn (x, _, _) => x = "Value") attrs of
                                                    ([(_, value, _)], rest) =>
                                                    (rest, SOME value)
                                                  | _ => (attrs, NONE))
                                             | "body" =>
                                               (attrs,
                                                if (case (#1 dynClass, #1 dynStyle) of
                                                        (L'.ESome _, _) => true
                                                      | (_, L'.ESome _) => true
                                                      | _ => false) then
                                                    let
                                                        fun jsify (e : L'.exp) =
                                                            case #1 e of
                                                                L'.ESome (_, ds) => strcat [str "execD(",
                                                                                            (L'.EJavaScript (L'.Script, ds), loc),
                                                                                            str ")"]
                                                              | _ => str "null"
                                                    in
                                                        SOME (strcat [str "<script type=\"text/javascript\">bodyDynClass(",
                                                                      jsify dynClass,
                                                                      str ",",
                                                                      jsify dynStyle,
				                                      str ")</script>"])
                                                    end
                                                else
                                                    NONE)
                                             | _ => (attrs, NONE)


                val dynamics = ["dyn", "ctextbox", "cpassword", "ccheckbox", "cradio", "cselect", "coption", "ctextarea", "active", "script", "cemail", "csearch", "curl", "ctel", "ccolor"]

                fun isSome (e, _) =
                    case e of
                        L'.ESome _ => true
                      | _ => false

                val () = if isSome dynClass orelse isSome dynStyle then
                             if List.exists (fn x => x = tag) dynamics then
                                 E.errorAt loc ("Dynamic tag <" ^ tag ^ "> cannot be combined with 'dynClass' or 'dynStyle' attribute; an additional <span> may be useful")
                             else
                                 ()
                         else
                             ()

                fun tagStart tag' =
                    let
                        val t = (L'.TFfi ("Basis", "string"), loc)
                        val s = strH (String.concat ["<", tag'])

                        val s = (L'.EStrcat (s,
                                             (L'.ECase (class,
                                                        [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                                                          strH ""),
                                                         ((L'.PVar ("x", t), loc),
                                                          (L'.EStrcat (strH " class=\"",
                                                                       (L'.EStrcat ((L'.ERel 0, loc),
                                                                                    strH "\""),
                                                                        loc)), loc))],
                                                        {disc = t,
                                                         result = t}), loc)), loc)

                        val s = (L'.EStrcat (s,
                                             (L'.ECase (style,
                                                        [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                                                          strH ""),
                                                         ((L'.PVar ("x", t), loc),
                                                          (L'.EStrcat (strH " style=\"",
                                                                       (L'.EStrcat ((L'.ERel 0, loc),
                                                                                    strH "\""),
                                                                        loc)), loc))],
                                                        {disc = t,
                                                         result = t}), loc)), loc)

                        val (s, fm) = foldl (fn (("Action", _, _), acc) => acc
                                              | (("Source", _, _), acc) => acc
                                              | (("Data", e, _), (s, fm)) =>
                                                ((L'.EStrcat (s,
                                                              (L'.EStrcat (
                                                               strH " ",
                                                               e), loc)), loc),
                                                 fm)
                                              | ((x, e, t), (s, fm)) =>
                                                case t of
                                                    (L'.TFfi ("Basis", "bool"), _) =>
                                                    let
                                                        val s' = " " ^ lowercaseFirst x
                                                    in
                                                        ((L'.ECase (e,
                                                                    [((L'.PCon (L'.Enum,
                                                                                L'.PConFfi {mod = "Basis",
                                                                                            datatyp = "bool",
                                                                                            con = "True",
                                                                                            arg = NONE},
                                                                                NONE), loc),
                                                                      (L'.EStrcat (s,
                                                                                   strH s'), loc)),
                                                                     ((L'.PCon (L'.Enum,
                                                                                L'.PConFfi {mod = "Basis",
                                                                                            datatyp = "bool",
                                                                                            con = "False",
                                                                                            arg = NONE},
                                                                                NONE), loc),
                                                                      s)],
                                                                    {disc = (L'.TFfi ("Basis", "bool"), loc),
                                                                     result = (L'.TFfi ("Basis", "string"), loc)}), loc),
                                                         fm)
                                                    end
                                                  | (L'.TFun (dom, _), _) =>
                                                    let
                                                        val e =
                                                            case #1 dom of
                                                                L'.TRecord [] => (L'.EApp (e, (L'.ERecord [], loc)), loc)
                                                              | _ =>
                                                                if String.isPrefix "Onkey" x then
                                                                    (L'.EApp ((L'.EApp (e, (L'.EFfiApp ("Basis", "keyEvent", []), loc)),
                                                                               loc), (L'.ERecord [], loc)), loc)
                                                                else
                                                                    (L'.EApp ((L'.EApp (e, (L'.EFfiApp ("Basis", "mouseEvent", []), loc)),
                                                                               loc), (L'.ERecord [], loc)), loc)

                                                        val s' = " " ^ lowercaseFirst x ^ "='uw_event=event;exec("
                                                    in
                                                        ((L'.EStrcat (s,
                                                                      (L'.EStrcat (
                                                                       strH s',
                                                                       (L'.EStrcat (
                                                                        (L'.EJavaScript (L'.Attribute, e), loc),
                                                                        strH ")'"), loc)),
                                                                       loc)), loc),
                                                         fm)
                                                    end
                                                  | _ =>
                                                    let
                                                        val fooify =
                                                            case x of
                                                                "Link" => urlifyExp
                                                              | "Action" => urlifyExp
                                                              | _ => attrifyExp

                                                        val x =
                                                            case x of
                                                                "Typ" => "Type"
                                                              | "Nam" => "Name"
                                                              | "Link" => "Href"
                                                              | _ => x

                                                        val x = String.translate (fn #"_" => "-"
                                                                                   | ch => String.str ch) x

                                                        val xp = " " ^ lowercaseFirst x ^ "=\""

                                                        val (e, fm) = fooify env fm (e, t)
                                                        val e = case (tag, x) of
                                                                    ("coption", "Value") => (L'.EStrcat (strH "x", e), loc)
                                                                  | _ => e
                                                    in
                                                        ((L'.EStrcat (s,
                                                                      (L'.EStrcat (strH xp,
                                                                                   (L'.EStrcat (e,
                                                                                                strH "\""),
                                                                                    loc)),
                                                                       loc)), loc),
                                                         fm)
                                                    end)
                                            (s, fm) attrs
                    in
                        (if tag = "coption" andalso List.all (fn ("Value", _, _) => false | _ => true) attrs then
                            (L'.EStrcat (s,
                                         strH " value=\"\""), loc)
                         else
                             s,
                         fm)
                    end

                fun input typ =
                    case targs of
                        [_, (L.CName name, _)] =>
                        let
                            val (ts, fm) = tagStart "input"
                        in
                            ((L'.EStrcat (ts,
                                          strH (" type=\"" ^ typ ^ "\" name=\"" ^ name ^ "\" />")), loc), fm)
                        end
                      | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                              raise Fail "No name passed to input tag")

                fun normal (tag, extra) =
                    let
                        val (tagStart, fm) = tagStart tag
                        val tagStart = case extra of
                                           NONE => tagStart
                                         | SOME extra => (L'.EStrcat (tagStart, extra), loc)

                        val firstWord = Substring.string o #1 o Substring.splitl (fn ch => not (Char.isSpace ch)) o Substring.full

                        fun normal () =
                            let
                                val (xml, fm) = monoExp (env, st, fm) xml

                                val xml = case extraString of
                                              NONE => xml
                                            | SOME extra => (L'.EStrcat (extra, xml), loc)
                            in
                                ((L'.EStrcat ((L'.EStrcat (tagStart, strH ">"), loc),
                                              (L'.EStrcat (xml,
                                                           strH (String.concat ["</", firstWord tag, ">"])), loc)),
                                  loc),
                                 fm)
                            end

                        fun isSingleton () =
                            let
                                val (bef, aft) = Substring.splitl (not o Char.isSpace) (Substring.full tag)
                            in
                                SS.member (singletons, if Substring.isEmpty aft then
                                                           tag
                                                       else
                                                           Substring.string bef)
                            end
                    in
                        case (xml, extraString) of
                            ((L.EApp ((L.ECApp (
                                       (L.ECApp ((L.EFfi ("Basis", "cdata"), _),
                                                 _), _),
                                       _), _),
                                      (L.EPrim (Prim.String (_, s)), _)), _), NONE) =>
                            if CharVector.all Char.isSpace s andalso isSingleton () then
                                ((L'.EStrcat (tagStart, strH " />"), loc), fm)
                            else
                                normal ()
                          | _ => normal ()
                    end

                fun setAttrs jexp =
                    let
                        val s = strH (String.concat ["<", tag])

                        val assgns = List.mapPartial
                                     (fn ("Source", _, _) => NONE
                                       | ("Onchange", e, _) =>
                                         SOME (strcat [str "addOnChange(d,exec(",
                                                       (L'.EJavaScript (L'.Script, e), loc),
                                                       str "));"])
                                       | ("Oninput", e, _) =>
                                         SOME (strcat [str "addOnInput(d,exec(",
                                                       (L'.EJavaScript (L'.Script, e), loc),
                                                       str "));"])
                                       | (x, e, (L'.TFun ((L'.TRecord [], _), _), _)) =>
                                         SOME (strcat [str ("d." ^ lowercaseFirst x ^ "=exec("),
                                                       (L'.EJavaScript (L'.Script, e), loc),
                                                       str ");"])
                                       | (x, e, _) =>
                                         if String.isPrefix "On" x then
                                             let
                                                 val arg = if String.isPrefix "Onkey" x then
                                                               SOME (L'.EFfiApp ("Basis", "keyEvent", []), loc)
                                                           else if String.isSuffix "click" x orelse String.isPrefix "Onmouse" x then
                                                               SOME (L'.EFfiApp ("Basis", "mouseEvent", []), loc)
                                                           else
                                                               NONE

                                                 val e = liftExpInExp 0 e

                                                 val e = case arg of
                                                             NONE => e
                                                           | SOME arg => (L'.EApp (e, arg), loc)

                                                 val e = (L'.EAbs ("_", (L'.TRecord [], loc), (L'.TRecord [], loc),
                                                                   (L'.EApp (e, (L'.ERecord [], loc)), loc)), loc)
                                             in
                                                 case x of
                                                     "Onkeyup" =>
                                                     SOME (strcat [str ("((function(c){addOnKeyUp(d,function(ev){window.uw_event=ev?ev:window.event;return c();});})(exec("),
                                                                   (L'.EJavaScript (L'.Script, e), loc),
                                                                   str ")));"])
                                                   | _ =>
                                                     SOME (strcat [str ("((function(c){d." ^ lowercaseFirst x ^ "=function(ev){window.uw_event=ev?ev:window.event;return c();};})(exec("),
                                                                   (L'.EJavaScript (L'.Script, e), loc),
                                                                   str ")));"])
                                             end
                                         else
                                             SOME (strcat [str ("d." ^ lowercaseFirst x ^ "=exec("),
                                                           (L'.EJavaScript (L'.Script, e), loc),
                                                           str ");"]))
                                     attrs

                        val t = (L'.TFfi ("Basis", "string"), loc)
                        val setClass = (L'.ECase (class,
                                                  [((L'.PPrim (Prim.String (Prim.Normal, "")), loc),
                                                    str ""),
                                                   ((L'.PVar ("x", t), loc),
                                                    (L'.EStrcat (strH "d.className=\"",
                                                                 (L'.EStrcat ((L'.ERel 0, loc),
                                                                              strH "\";"), loc)),
                                                     loc))],
                                                  {disc = (L'.TOption t, loc),
                                                   result = t}), loc)
                    in
                        case assgns of
                            [] => strcat [str "var d=",
                                          jexp,
                                          str ";",
                                          setClass]
                          | _ => strcat (str "var d="
                                         :: jexp
                                         :: str ";"
                                         :: setClass
                                         :: assgns)
                    end

                fun execify e =
                    case e of
                        NONE => strH ""
                      | SOME e =>
                        let
                            val e = (L'.EApp (e, (L'.ERecord [], loc)), loc)
                        in
                            (L'.EStrcat (strH "exec(",
                                         (L'.EStrcat ((L'.EJavaScript (L'.Attribute, e), loc),
                                                      strH ")"), loc)), loc)
                        end

                fun inTag tag' = case ctxOuter of
				     (L.CRecord (_, ctx), _) =>
				     List.exists (fn ((L.CName tag'', _), _) => tag'' = tag'
                                                   | _ => false) ctx
                                  | _ => false

                fun pnode () = if inTag "Tr" then
			           "tr"
                               else if inTag "Table" then
			           "table"
                               else
			           "span"

                fun cinput (fallback, dynamic) =
		    case List.find (fn ("Source", _, _) => true | _ => false) attrs of
                        NONE =>
                        let
			    val (ts, fm) = tagStart "input"
                        in
			    ((L'.EStrcat (ts,
                                          strH (" type=\"" ^ fallback ^ "\" />")),
                              loc), fm)
                        end
                      | SOME (_, src, _) =>
                        let
			    val sc = strcat [str (dynamic ^ "(exec("),
					     (L'.EJavaScript (L'.Script, src), loc),
					     str "))"]
			    val sc = setAttrs sc
                        in
			    (strcat [str "<script type=\"text/javascript\">",
				     sc,
				     str "</script>"],
			     fm)
                        end

		val baseAll as (base, fm) =
                    case tag of
			"body" => let
                            val onload = execify onload
                            val onunload = execify onunload
                            val s = (L'.TFfi ("Basis", "string"), loc)
			in
                            normal ("body",
                                    SOME (L'.EStrcat ((L'.EFfiApp ("Basis", "maybe_onload",
                                                                   [((L'.EStrcat ((L'.EFfiApp ("Basis", "get_settings",
                                                                                               [((L'.ERecord [], loc),
                                                                                                 (L'.TRecord [], loc))]), loc),
										  onload), loc),
                                                                     s)]),
                                                       loc),
                                                      (L'.EFfiApp ("Basis", "maybe_onunload",
                                                                   [(onunload, s)]),
                                                       loc)), loc))
			end

                      | "dyn" =>
			let
			in
                            case attrs of
				[("Signal", e, _)] =>
				((L'.EStrcat
                                      (strH ("<script type=\"text/javascript\">dyn(\""
                                             ^ pnode () ^ "\", execD("),
                                       (L'.EStrcat ((L'.EJavaScript (L'.Script, e), loc),
                                                    strH ("))</script>")), loc)), loc),
				 fm)
                              | _ => raise Fail "Monoize: Bad <dyn> attributes"
			end

                      | "active" =>
                        (case attrs of
			     [("Code", e, _)] =>
			     ((L'.EStrcat
                                   (strH "<script type=\"text/javascript\">active(execD(",
                                    (L'.EStrcat ((L'.EJavaScript (L'.Script, e), loc),
                                                 strH "))</script>"), loc)), loc),
			      fm)
                           | _ => raise Fail "Monoize: Bad <active> attributes")

                      | "script" =>
                        (case attrs of
			     [("Code", e, _)] =>
			     ((L'.EStrcat
                                   (strH "<script type=\"text/javascript\">execF(execD(",
                                    (L'.EStrcat ((L'.EJavaScript (L'.Script, e), loc),
                                                 strH "))</script>"), loc)), loc),
			      fm)
                           | _ => raise Fail "Monoize: Bad <script> attributes")

                      | "submit" => normal ("input type=\"submit\"", NONE)
                      | "image" => normal ("input type=\"image\"", NONE)
                      | "hidden" => input "hidden"

                      | "textbox" =>
			(case targs of
                             [_, (L.CName name, _)] =>
                             (case List.find (fn ("Source", _, _) => true | _ => false) attrs of
				  NONE =>
				  let
                                      val (ts, fm) = tagStart "input"
				  in
                                      ((L'.EStrcat (ts,
                                                    strH (" type=\"text\" name=\"" ^ name ^ "\" />")),
                                        loc), fm)
				  end
				| SOME (_, src, _) =>
				  (strcat [str "<script type=\"text/javascript\">inp(exec(",
                                           (L'.EJavaScript (L'.Script, src), loc),
                                           str "), \"",
                                           str name,
                                           str "\")</script>"],
                                   fm))
                           | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                                   raise Fail "No name passed to textbox tag"))
                      | "password" => input "password"
                      | "email" => input "email"
                      | "search" => input "search"
                      | "url_" => input "url"
                      | "tel" => input "tel"
                      | "color" => input "color"
                      | "number" => input "number"
                      | "range" => input "range"
                      | "date" => input "date"
                      | "datetime" => input "datetime"
                      | "datetime_local" => input "datetime-local"
                      | "month" => input "month"
                      | "week" => input "week"
                      | "timeInput" => input "time"
                      | "textarea" =>
			(case targs of
                             [_, (L.CName name, _)] =>
                             let
				 val (ts, fm) = tagStart "textarea"
				 val (xml, fm) = monoExp (env, st, fm) xml
                             in
				 ((L'.EStrcat ((L'.EStrcat (ts,
                                                            strH (" name=\"" ^ name ^ "\">")), loc),
                                               (L'.EStrcat (xml,
                                                            strH "</textarea>"), loc)),
                                   loc), fm)
                             end
                           | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                                   raise Fail "No name passed to ltextarea tag"))

                      | "checkbox" => input "checkbox"
                      | "upload" => input "file"

                      | "radio" =>
			(case targs of
                             [_, (L.CName name, _)] =>
                             monoExp (env, St.setRadioGroup (st, name), fm) xml
                           | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                                   raise Fail "No name passed to radio tag"))
                      | "radioOption" =>
			(case St.radioGroup st of
                             NONE => raise Fail "No name for radioGroup"
                           | SOME name =>
                             normal ("input",
                                     SOME (strH (" type=\"radio\" name=\"" ^ name ^ "\""))))

                      | "select" =>
			(case targs of
                             [_, (L.CName name, _)] =>
                             let
				 val (ts, fm) = tagStart "select"
				 val (xml, fm) = monoExp (env, st, fm) xml
                             in
				 ((L'.EStrcat ((L'.EStrcat (ts,
                                                            strH (" name=\"" ^ name ^ "\">")), loc),
                                               (L'.EStrcat (xml,
                                                            strH "</select>"),
                                                loc)),
                                   loc),
				  fm)
                             end
                           | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                                   raise Fail "No name passed to lselect tag"))

                      | "ctextbox" => cinput ("text", "inp")
                      | "cpassword" => cinput ("password", "password")
                      | "cemail" => cinput ("email", "email")
                      | "csearch" => cinput ("search", "search")
                      | "curl" => cinput ("url", "url")
                      | "ctel" => cinput ("tel", "tel")
                      | "ccolor" => cinput ("color", "color")

                      | "cnumber" => cinput ("number", "number")
                      | "crange" => cinput ("range", "range")
                      | "cdate" => cinput ("date", "date")
                      | "cdatetime" => cinput ("datetime", "datetime")
                      | "cdatetime_local" => cinput ("datetime-local", "datetime_local")
                      | "cmonth" => cinput ("month", "month")
                      | "cweek" => cinput ("week", "week")
                      | "ctime" => cinput ("time", "time")

                      | "ccheckbox" => cinput ("checkbox", "chk")
                      | "cradio" => cinput ("radio", "crad")

                      | "cselect" =>
			(case List.find (fn ("Source", _, _) => true | _ => false) attrs of
                             NONE =>
                             let
				 val (xml, fm) = monoExp (env, st, fm) xml
				 val (ts, fm) = tagStart "select"
                             in
				 (strcat [ts,
					  str ">",
					  xml,
					  str "</select>"],
				  fm)
                             end
                           | SOME (_, src, _) =>
                             let
				 val (xml, fm) = monoExp (env, st, fm) xml

				 val sc = strcat [str "sel(exec(",
						  (L'.EJavaScript (L'.Script, src), loc),
						  str "),exec(",
						  (L'.EJavaScript (L'.Script, xml), loc),
						  str "))"]
				 val sc = setAttrs sc
                             in
				 (strcat [str "<script type=\"text/javascript\">",
					  sc,
					  str "</script>"],
				  fm)
                             end)

                      | "coption" => normal ("option", NONE)

                      | "ctextarea" =>
			(case List.find (fn ("Source", _, _) => true | _ => false) attrs of
                             NONE =>
                             let
				 val (ts, fm) = tagStart "textarea"
                             in
				 ((L'.EStrcat (ts,
                                               strH " />"),
                                   loc), fm)
                             end
                           | SOME (_, src, _) =>
                             let
				 val sc = strcat [str "tbx(exec(",
						  (L'.EJavaScript (L'.Script, src), loc),
						  str "))"]
				 val sc = setAttrs sc
                             in
				 (strcat [str "<script type=\"text/javascript\">",
					  sc,
					  str "</script>"],
				  fm)
                             end)

                      | "tabl" => normal ("table", NONE)
                      | _ => normal (tag, NONE)

                val (dynClass', dynStyle') =
                    case tag of
                        "body" => ((L'.ENone dummyTyp, ErrorMsg.dummySpan),
                                   (L'.ENone dummyTyp, ErrorMsg.dummySpan))
                      | _ => (dynClass, dynStyle)
	    in
		case #1 dynClass' of
		    L'.ENone _ =>
		    (case #1 dynStyle' of
		         L'.ENone _ => baseAll
		       | L'.ESome (_, ds) => (strcat [str "<script type=\"text/javascript\">dynClass(\"",
                                                      str (pnode ()),
                                                      str "\",execD(",
				                      (L'.EJavaScript (L'.Script, base), loc),
				                      str "),null,execD(",
				                      (L'.EJavaScript (L'.Script, ds), loc),
				                      str "))</script>"],
			                      fm)
                       | _ => (E.errorAt loc "Absence/presence of 'dynStyle' unknown";
                               baseAll))
		  | L'.ESome (_, dc) =>
                    let
                        val e = case #1 dynStyle' of
                                    L'.ENone _ => str "null"
                                  | L'.ESome (_, ds) => strcat [str "execD(",
                                                                (L'.EJavaScript (L'.Script, ds), loc),
                                                                str ")"]
                                  | _ => (E.errorAt loc "Absence/presence of 'dynStyle' unknown";
                                          str "null")
                    in
                        (strcat [str "<script type=\"text/javascript\">dynClass(\"",
                                 str (pnode ()),
                                 str "\",execD(",
				 (L'.EJavaScript (L'.Script, base), loc),
				 str "),execD(",
				 (L'.EJavaScript (L'.Script, dc), loc),
				 str "),",
                                 e,
                                 str ")</script>"],
			 fm)
                    end
                  | _ => (E.errorAt loc "Absence/presence of 'dynClass' unknown";
                          baseAll)
            end

          | L.EApp (
            (L.EApp ((L.EApp ((L.ECApp (
                                    (L.ECApp ((L.EFfi ("Basis", "form"), _), _), _),
                                    (L.CRecord (_, fields), _)), _),
                              id), _),
                     class), _),
            xml) =>
            let
                fun findSubmit (e, _) =
                    case e of
                        L.EApp (
                        (L.EApp (
                         (L.ECApp (
                          (L.ECApp (
                           (L.ECApp (
                            (L.ECApp (
                             (L.EFfi ("Basis", "join"),
                              _), _), _),
                            _), _),
                           _), _),
                          _), _),
                         xml1), _),
                        xml2) => (case findSubmit xml1 of
                                      Error => Error
                                    | NotFound => findSubmit xml2
                                    | Found e =>
                                      case findSubmit xml2 of
                                          NotFound => Found e
                                        | _ => Error)
                      | L.EApp (
                        (L.EApp (
                         (L.EApp (
                          (L.EApp (
                           (L.EApp (
                            (L.EApp (
                             (L.EApp (
                              (L.ECApp (
                               (L.ECApp (
                                (L.ECApp (
                                 (L.ECApp (
                                  (L.ECApp (
                                   (L.ECApp (
                                    (L.ECApp (
                                     (L.ECApp (
                                      (L.EFfi ("Basis", "tag"),
                                       _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _),
                              _), _),
                             _), _),
                            _), _),
                           _), _),
                          attrs), _),
                         _), _),
                        xml) =>
                        (case #1 attrs of
                             L.ERecord xes =>
                             (case ListUtil.search (fn ((L.CName "Action", _), e, t) => SOME (e, t)
                                                     | _ => NONE) xes of
                                  NONE => findSubmit xml
                                | SOME et =>
                                  case findSubmit xml of
                                      NotFound => Found et
                                    | _ => Error)
                           | _ => findSubmit xml)
                      | _ => NotFound

                val (func, action, fm) = case findSubmit xml of
                    NotFound => (0, strH "", fm)
                  | Error => raise Fail "Not ready for multi-submit lforms yet"
                  | Found (action, actionT) =>
                    let
                        val func = case #1 action of
                                       L.EClosure (n, _) => n
                                     | _ => raise Fail "Monoize: Action is not a closure"
                        val actionT = monoType env actionT
                        val (action, fm) = monoExp (env, st, fm) action
                        val (action, fm) = urlifyExp env fm (action, actionT)
                    in
                        (func,
                         (L'.EStrcat (strH " action=\"",
                                      (L'.EStrcat (action,
                                                   strH "\""), loc)), loc),
                         fm)
                    end

                val hasUpload = CoreUtil.Exp.exists {kind = fn _ => false,
                                                     con = fn _ => false,
                                                     exp = fn e =>
                                                              case e of
                                                                  L.EFfi ("Basis", "upload") => true
                                                                | _ => false} xml

                val (xml, fm) = monoExp (env, st, fm) xml

                val xml =
                    if IS.member (!readCookie, func) then
                        let
                            fun inFields s = List.exists (fn ((L.CName s', _), _) => s' = s
                                                           | _ => true) fields

                            fun getSigName () =
                                let
                                    fun getSigName' n =
                                        let
                                            val s = "Sig" ^ Int.toString n
                                        in
                                            if inFields s then
                                                getSigName' (n + 1)
                                            else
                                                s
                                        end
                                in
                                    if inFields "Sig" then
                                        getSigName' 0
                                    else
                                        "Sig"
                                end

                            val sigName = getSigName ()
                            val sigSet = (L'.EFfiApp ("Basis", "sigString", [((L'.ERecord [], loc), (L'.TRecord [], loc))]), loc)
                            val sigSet = (L'.EStrcat (strH ("<input type=\"hidden\" name=\""
                                                            ^ sigName
                                                            ^ "\" value=\""),
                                                      sigSet), loc)
                            val sigSet = (L'.EStrcat (sigSet,
                                                      strH "\" />"), loc)
                        in
                            (L'.EStrcat (sigSet, xml), loc)
                        end
                    else
                        xml

                val action = if hasUpload then
                                 (L'.EStrcat (action,
                                              strH " enctype=\"multipart/form-data\""), loc)
                             else
                                 action

                val stt = (L'.TFfi ("Basis", "string"), loc)
                val (id, fm) = monoExp (env, st, fm) id
                val (class, fm) = monoExp (env, st, fm) class
                val action = (L'.EStrcat (action,
                                          (L'.ECase (class,
                                                     [((L'.PNone stt, loc),
                                                       strH ""),
                                                      ((L'.PSome (stt, (L'.PVar ("x", stt), loc)), loc),
                                                       (L'.EStrcat (strH " class=\"",
                                                                    (L'.EStrcat ((L'.ERel 0, loc),
                                                                                 strH "\""), loc)), loc))],
                                                     {disc = (L'.TOption stt, loc),
                                                      result = stt}), loc)), loc)
            in
                ((L'.EStrcat ((L'.EStrcat (strH "<form method=\"post\"",
                                           (L'.EStrcat ((L'.ECase (id,
                                                                   [((L'.PNone stt, loc),
                                                                     strH ""),
                                                                    ((L'.PSome (stt, (L'.PVar ("id", stt), loc)), loc),
                                                                     (L'.EStrcat (strH " id=\"",
                                                                                  (L'.EStrcat ((L'.ERel 0, loc),
                                                                                               strH "\""), loc)), loc))],
                                                                   {disc = (L'.TOption stt, loc),
                                                                    result = stt}), loc),
                                                        (L'.EStrcat (action,
                                                                     strH ">"), loc)), loc)), loc),
                              (L'.EStrcat (xml,
                                           strH "</form>"), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.ECApp ((L.ECApp ((L.ECApp (
                                          (L.EFfi ("Basis", "subform"), _), _), _), _),
                                _), _), _), (L.CName nm, loc)) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("xml", s, s,
                           strcat [strH ("<input type=\"hidden\" name=\".b\" value=\""
                                         ^ nm ^ "\" />"),
                                   (L'.ERel 0, loc),
                                   strH ("<input type=\"hidden\" name=\".e\" value=\"1\" />")]),
                  loc),
                 fm)
            end

          | L.ECApp ((L.ECApp ((L.ECApp ((L.ECApp (
                                          (L.EFfi ("Basis", "subforms"), _), _), _), _),
                                _), _), _), (L.CName nm, loc)) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("xml", s, s,
                           strcat [strH ("<input type=\"hidden\" name=\".s\" value=\""
                                         ^ nm ^ "\" />"),
                                   (L'.ERel 0, loc),
                                   strH ("<input type=\"hidden\" name=\".e\" value=\"1\" />")]),
                  loc),
                 fm)
            end

          | L.ECApp ((L.ECApp (
                      (L.EFfi ("Basis", "entry"), _), _), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("xml", s, s,
                           strcat [strH ("<input type=\"hidden\" name=\".i\" value=\"1\" />"),
                                   (L'.ERel 0, loc),
                                   strH ("<input type=\"hidden\" name=\".e\" value=\"1\" />")]),
                  loc),
                 fm)
            end

          | L.EApp ((L.ECApp (
                     (L.ECApp (
                      (L.ECApp (
                       (L.ECApp (
                        (L.EFfi ("Basis", "useMore"), _), _), _),
                       _), _),
                      _), _),
                     _), _),
                    xml) => monoExp (env, st, fm) xml

          | L.ECApp ((L.EFfi ("Basis", "error"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("s", (L'.TFfi ("Basis", "string"), loc), t,
                           (L'.EError ((L'.ERel 0, loc), t), loc)), loc),
                 fm)
            end
          | L.EApp (
            (L.ECApp ((L.EFfi ("Basis", "returnBlob"), _), t), _),
            (L.EFfiApp ("Basis", "textBlob", [(e, _)]), _)) =>
            let
                val t = monoType env t
                val un = (L'.TRecord [], loc)
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.EAbs ("mt", (L'.TFfi ("Basis", "string"), loc), (L'.TFun (un, t), loc),
                           (L'.EAbs ("_", un, t,
                                     (L'.ESeq ((L'.EFfiApp ("Basis", "clear_page", []), loc),
                                               (L'.ESeq ((L'.EWrite (liftExpInExp 0 (liftExpInExp 0 e)), loc),
                                                         (L'.EReturnBlob {blob = NONE,
                                                                          mimeType = (L'.ERel 1, loc),
                                                                          t = t}, loc)), loc)), loc)), loc)),
                  loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "returnBlob"), _), t) =>
            let
                val t = monoType env t
                val un = (L'.TRecord [], loc)
            in
                ((L'.EAbs ("b", (L'.TFfi ("Basis", "blob"), loc),
                           (L'.TFun ((L'.TFfi ("Basis", "string"), loc), (L'.TFun (un, t), loc)), loc),
                           (L'.EAbs ("mt", (L'.TFfi ("Basis", "string"), loc), (L'.TFun (un, t), loc),
                                     (L'.EAbs ("_", un, t,
                                               (L'.EReturnBlob {blob = SOME (L'.ERel 2, loc),
                                                                mimeType = (L'.ERel 1, loc),
                                                                t = t}, loc)), loc)), loc)), loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "redirect"), _), t) =>
            let
                val t = monoType env t
                val un = (L'.TRecord [], loc)
            in
                ((L'.EAbs ("url", (L'.TFfi ("Basis", "string"), loc), (L'.TFun (un, t), loc),
                           (L'.EAbs ("_", un, t,
                                     (L'.ERedirect ((L'.ERel 1, loc), t), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "serialize"), _), t) =>
            let
                val t = monoType env t
                val (e, fm) = urlifyExp env fm ((L'.ERel 0, loc), t)
            in
                ((L'.EAbs ("v", t, (L'.TFfi ("Basis", "string"), loc), e), loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "deserialize"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("v", (L'.TFfi ("Basis", "string"), loc), t, (L'.EUnurlify ((L'.ERel 0, loc), t, false),
                                                                        loc)), loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "unsafeSerializedToString"), _), _) =>
            let
                val t = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("v", t, t, (L'.ERel 0, loc)), loc),
                 fm)
            end
          | L.ECApp ((L.EFfi ("Basis", "unsafeSerializedFromString"), _), _) =>
            let
                val t = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EAbs ("v", t, t, (L'.ERel 0, loc)), loc),
                 fm)
            end

          | L.EFfiApp ("Basis", "url", [(e, _)]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
                val (e, fm) = urlifyExp env fm (e, dummyTyp)
            in
                ((L'.EStrcat (str (Settings.getUrlPrePrefix ()), e), loc), fm)
            end

          | L.EApp (e1, e2) =>
            let
                val (e1, fm) = monoExp (env, st, fm) e1
                val (e2, fm) = monoExp (env, st, fm) e2
            in
                ((L'.EApp (e1, e2), loc), fm)
            end
          | L.EAbs (x, dom, ran, e) =>
            let
                val (e, fm) = monoExp (Env.pushERel env x dom, st, fm) e
            in
                ((L'.EAbs (x, monoType env dom, monoType env ran, e), loc), fm)
            end

          | L.ECApp (e, _) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                case #1 e of
                    L'.EFfi _ => (e, fm)
                  | _ => poly ()
            end
          | L.ECAbs _ => poly ()

          | L.EFfi mx => ((L'.EFfi mx, loc), fm)
          | L.EFfiApp (m, x, es) =>
            let
                val (es, fm) = ListUtil.foldlMap (fn ((e, t), fm) =>
                                                     let
                                                         val (e, fm) = monoExp (env, st, fm) e
                                                     in
                                                         ((e, monoType env t), fm)
                                                     end) fm es
            in
                ((L'.EFfiApp (m, x, es), loc), fm)
            end

          | L.ERecord xes =>
            let
                val (xes, fm) = ListUtil.foldlMap
                                    (fn ((x, e, t), fm) =>
                                        let
                                            val (e, fm) = monoExp (env, st, fm) e
                                        in
                                            ((monoName env x,
                                              e,
                                              monoType env t), fm)
                                        end) fm xes

                val xes = ListMergeSort.sort (fn ((x, _, _), (y, _, _)) => String.compare (x, y) = GREATER) xes
            in
                ((L'.ERecord xes, loc), fm)
            end
          | L.EField (e, x, _) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.EField (e, monoName env x), loc), fm)
            end
          | L.EConcat _ => poly ()
          | L.ECut _ => poly ()
          | L.ECutMulti _ => poly ()

          | L.ECase (e, pes, {disc, result}) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
                val (pes, fm) = ListUtil.foldlMap
                                    (fn ((p, e), fm) =>
                                        let
                                            val (e, fm) = monoExp (env, st, fm) e
                                        in
                                            ((monoPat env p, e), fm)
                                        end) fm pes
            in
                ((L'.ECase (e, pes, {disc = monoType env disc, result = monoType env result}), loc), fm)
            end

          | L.EWrite e =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TRecord [], loc),
                           (L'.EWrite (liftExpInExp 0 e), loc)), loc), fm)
            end

          | L.EClosure (n, es) =>
            let
                val (es, fm) = ListUtil.foldlMap (fn (e, fm) =>
                                                     monoExp (env, st, fm) e)
                                                 fm es
                val e = (L'.EClosure (n, es), loc)
            in
                (e, fm)
            end

          | L.ELet (x, t, e1, e2) =>
            let
                val t' = monoType env t
                val (e1, fm) = monoExp (env, st, fm) e1
                val (e2, fm) = monoExp (Env.pushERel env x t, st, fm) e2
            in
                ((L'.ELet (x, t', e1, e2), loc), fm)
            end

          | L.EServerCall (n, es, t, fmode) =>
            let
                val t = monoType env t
                val (_, ft, _, name) = Env.lookupENamed env n
                val (es, fm) = ListUtil.foldlMap (fn (e, fm) => monoExp (env, st, fm) e) fm es

                fun encodeArgs (es, ft, acc, fm) =
                    case (es, ft) of
                        ([], _) => (rev acc, fm)
                      | (e :: es, (L.TFun (dom, ran), _)) =>
                        let
                            val (e, fm) = urlifyExp env fm (e, monoType env dom)
                        in
                            encodeArgs (es, ran, e
                                                 :: str "/"
                                                 :: acc, fm)
                        end
                      | _ => raise Fail "Monoize: Not enough arguments visible in RPC function type"

                val (call, fm) = encodeArgs (es, ft, [], fm)
                val call = foldl (fn (e, call) => (L'.EStrcat (call, e), loc))
                                 (str name) call

                val unit = (L'.TRecord [], loc)

                val eff = if IS.member (!readCookie, n) then
                              L'.ReadCookieWrite
                          else
                              L'.ReadOnly

                val e = (L'.EServerCall (call, t, eff, fmode), loc)
                val e = liftExpInExp 0 e
                val e = (L'.EAbs ("_", unit, unit, e), loc)
            in
                (e, fm)
            end

          | L.EKAbs _ => poly ()
          | L.EKApp _ => poly ()
    end

fun monoDecl (env, fm) (all as (d, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported declaration";
             Print.eprefaces' [("Declaration", CorePrint.p_decl env all)];
             NONE)

        fun str s = (L'.EPrim (Prim.String (Prim.Normal, s)), loc)
        fun strH s = (L'.EPrim (Prim.String (Prim.Html, s)), loc)
    in
        case d of
            L.DCon _ => NONE
          | L.DDatatype [("list", n, [_], [("Nil", _, NONE),
                                           ("Cons", _, SOME (L.TRecord (L.CRecord (_,
                                                                                   [((L.CName "1", _),
                                                                                     (L.CRel 0, _)),
                                                                                    ((L.CName "2", _),
                                                                                     (L.CApp ((L.CNamed n', _),
                                                                                              (L.CRel 0, _)),
                                                                                      _))]), _), _))])] =>
            if n = n' then
                NONE
            else
                poly ()
          | L.DDatatype dts =>
            let
                val env' = Env.declBinds env all
                val dts = map (fn (x, n, [], xncs) =>
                                  (x, n, map (fn (x, n, to) => (x, n, Option.map (monoType env') to)) xncs)
                                | _ => (E.errorAt loc "Polymorphic datatype needed too late";
                                        Print.eprefaces' [("Declaration", CorePrint.p_decl env all)];
                                        ("", 0, []))) dts
                val d = (L'.DDatatype dts, loc)
            in
                SOME (env', fm, [d])
            end
          | L.DVal (x, n, t, e, s) =>
            let
                val (e, fm) = monoExp (env, St.empty, fm) e
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DVal (x, n, monoType env t, e, s), loc)])
            end
          | L.DValRec vis =>
            let
                val vis = map (fn (x, n, t, e, s) =>
                                  let
                                      fun maybeTransaction (t, e) =
                                          case (#1 t, #1 e) of
                                              (L.CApp ((L.CFfi ("Basis", "transaction"), _), _), _) =>
                                              SOME (L.EAbs ("_",
                                                            (L.TRecord (L.CRecord ((L.KType, loc), []), loc), loc),
                                                            t,
                                                            (L.EApp (CoreEnv.liftExpInExp 0 e,
                                                                     (L.ERecord [], loc)), loc)), loc)
                                            | (L.TFun (dom, ran), L.EAbs (x, _, _, e)) =>
                                              (case maybeTransaction (ran, e) of
                                                   NONE => NONE
                                                 | SOME e => SOME (L.EAbs (x, dom, ran, e), loc))
                                            | _ => NONE
                                  in
                                      (x, n, t,
                                       case maybeTransaction (t, e) of
                                           NONE => e
                                         | SOME e => e,
                                       s)
                                  end) vis

                val env = foldl (fn ((x, n, t, e, s), env) => Env.pushENamed env x n t NONE s) env vis

                val (vis, fm) = ListUtil.foldlMap
                                    (fn ((x, n, t, e, s), fm) =>
                                        let
                                            val (e, fm) = monoExp (env, St.empty, fm) e
                                        in
                                            ((x, n, monoType env t, e, s), fm)
                                        end)
                                    fm vis
            in
                SOME (env,
                      fm,
                      [(L'.DValRec vis, loc)])
            end
          | L.DExport (ek, n, b) =>
            let
                val (_, t, _, s) = Env.lookupENamed env n
                val () = addPrefix s

                fun unwind (t, args) =
                    case #1 t of
                        L.TFun (dom, ran) => unwind (ran, dom :: args)
                      | L.CApp ((L.CFfi ("Basis", "transaction"), _), t) =>
                        unwind (t, (L.TRecord (L.CRecord ((L.KType, loc), []), loc), loc) :: args)
                      | _ => (rev args, t)

                val (ts, ran) = unwind (t, [])
                val ts = map (monoType env) ts
                val ran = monoType env ran
            in
                SOME (env, fm, [(L'.DExport (ek, s, n, ts, ran, b), loc)])
            end
          | L.DTable (x, n, (L.CRecord (_, xts), _), s, pe, _, ce, _) =>
            let
                val t = (L.CFfi ("Basis", "string"), loc)
                val t' = (L'.TFfi ("Basis", "string"), loc)
                val s = Settings.mangleSqlTable s
                val e_name = str s

                val xts = map (fn (x, t) => (monoName env x, monoType env t)) xts

                val (pe, fm) = monoExp (env, St.empty, fm) pe
                val (ce, fm) = monoExp (env, St.empty, fm) ce
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DTable (s, xts, pe, ce), loc),
                       (L'.DVal (x, n, t', e_name, s), loc)])
            end
          | L.DTable _ => poly ()
          | L.DView (x, n, s, e, (L.CRecord (_, xts), _)) =>
            let
                val t = (L.CFfi ("Basis", "string"), loc)
                val t' = (L'.TFfi ("Basis", "string"), loc)
                val s = Settings.mangleSqlTable s
                val e_name = str s

                val xts = map (fn (x, t) => (monoName env x, monoType env t)) xts

                val (e, fm) = monoExp (env, St.empty, fm) e
                val e = (L'.EFfiApp ("Basis", "viewify", [(e, t')]), loc)
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DView (s, xts, e), loc),
                       (L'.DVal (x, n, t', e_name, s), loc)])
            end
          | L.DView _ => poly ()
          | L.DIndex ((L.ENamed tab, _), (L.ERecord xms, _)) =>
            let
                val (_, _, _, path) = Env.lookupENamed env tab

                val failed = ref false
                val xms = List.mapPartial (fn (x, m, _) =>
                                              case #1 x of
                                                  L.CName x =>
                                                  (case #1 m of
                                                       L.ECApp ((L.EFfi ("Basis", "equality"), _), _) => SOME (x, L'.Equality)
                                                     | L.EApp ((L.ECApp ((L.EFfi ("Basis", "trigram"), _), _), _), _) => SOME (x, L'.Trigram)
                                                     | L.ECApp ((L.EFfi ("Basis", "skipped"), _), _) => SOME (x, L'.Skipped)
                                                     | _ => (failed := true; NONE))
                                                | _ => (failed := true; NONE)) xms
            in
                if !failed then
                    poly ()
                else
                    SOME (env, fm, [(L'.DIndex (path, xms), loc)])
            end
          | L.DIndex _ => poly ()
          | L.DSequence (x, n, s) =>
            let
                val t = (L.CFfi ("Basis", "string"), loc)
                val t' = (L'.TFfi ("Basis", "string"), loc)
                val s = Settings.mangleSql s
                val e = str s
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DSequence s, loc),
                       (L'.DVal (x, n, t', e, s), loc)])
            end
          | L.DDatabase _ => NONE
          | L.DCookie (x, n, t, s) =>
            let
                val t = (L.CFfi ("Basis", "string"), loc)
                val t' = (L'.TFfi ("Basis", "string"), loc)
                val e = str s
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DCookie s, loc),
                       (L'.DVal (x, n, t', e, s), loc)])
            end
          | L.DStyle (x, n, s) =>
            let
                val t = (L.CFfi ("Basis", "string"), loc)
                val t' = (L'.TFfi ("Basis", "string"), loc)
                val e = strH s
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DStyle s, loc),
                       (L'.DVal (x, n, t', e, s), loc)])
            end
          | L.DTask (e1, e2) =>
            let
                val (e1, fm) = monoExp (env, St.empty, fm) e1
                val (e2, fm) = monoExp (env, St.empty, fm) e2

                val un = (L'.TRecord [], loc)
                val t = if MonoUtil.Exp.exists {typ = fn _ => false,
                                                exp = fn L'.EFfiApp ("Basis", "periodic", _) =>
                                                         (if #persistent (Settings.currentProtocol ()) then
                                                              ()
                                                          else
                                                              E.errorAt (#2 e1)
                                                                        ("Periodic tasks aren't allowed in the selected protocol (" ^ #name (Settings.currentProtocol ()) ^ ").");
                                                          true)
                                                       | _ => false} e1 then
                            (L'.TFfi ("Basis", "int"), loc)
                        else
                            un

                val e2 = (L'.EAbs ("$x", t, (L'.TFun (un, un), loc),
                                   (L'.EAbs ("$y", un, un,
                                             (L'.EApp (
                                              (L'.EApp (e2, (L'.ERel 1, loc)), loc),
                                              (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                SOME (env,
                      fm,
                      [(L'.DTask (e1, e2), loc)])
            end
          | L.DPolicy e =>
            let
                fun policies (e, fm) =
                    case #1 e of
                        L.EFfiApp ("Basis", "also", [(e1, _), (e2, _)]) =>
                        let
                            val (ps1, fm) = policies (e1, fm)
                            val (ps2, fm) = policies (e2, fm)
                        in
                            (ps1 @ ps2, fm)
                        end
                      | _ =>
                        let
                            val (e, make) =
                                case #1 e of
                                    L.EApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sendClient"), _), _), _), _), _), e) =>
                                    (e, L'.PolClient)
                                  | L.EApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "mayInsert"), _), _), _), _), _), e) =>
                                    (e, L'.PolInsert)
                                  | L.EApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "mayDelete"), _), _), _), _), _), e) =>
                                    (e, L'.PolDelete)
                                  | L.EApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "mayUpdate"), _), _), _), _), _), e) =>
                                    (e, L'.PolUpdate)
                                  | L.EFfiApp ("Basis", "sendOwnIds", [(e, _)]) =>
                                    (e, L'.PolSequence)
                                  | _ => (poly (); (e, L'.PolClient))

                            val (e, fm) = monoExp (env, St.empty, fm) e
                        in
                            ([(L'.DPolicy (make e), loc)], fm)
                        end

                val (ps, fm) = policies (e, fm)
            in
                SOME (env, fm, ps)
            end
          | L.DOnError n => SOME (env,
                                  fm,
                                  [(L'.DOnError n, loc)])
    end

datatype expungable = Client | Channel

fun monoize env file =
    let
        val () = reset ()
        val () = pvars := RM.empty

        (* Calculate which exported functions need cookie signature protection *)
        val rcook = foldl (fn ((d, _), rcook) =>
                              case d of
                                  L.DExport (L.Action L.ReadCookieWrite, n, _) => IS.add (rcook, n)
                                | L.DExport (L.Rpc L.ReadCookieWrite, n, _) => IS.add (rcook, n)
                                | _ => rcook)
                          IS.empty file
        val () = readCookie := rcook

        val loc = E.dummySpan
        val client = (L'.TFfi ("Basis", "client"), loc)
        val unit = (L'.TRecord [], loc)

        fun str s = (L'.EPrim (Prim.String (Prim.Normal, s)), loc)
        fun strH s = (L'.EPrim (Prim.String (Prim.Html, s)), loc)

        fun calcClientish xts =
            foldl (fn ((x : L.con, t : L.con), st as (nullable, notNullable)) =>
                      case #1 x of
                          L.CName x =>
                          (case #1 t of
                               L.CFfi ("Basis", "client") =>
                               (nullable, (x, Client) :: notNullable)
                             | L.CApp ((L.CFfi ("Basis", "option"), _),
                                       (L.CFfi ("Basis", "client"), _)) =>
                               ((x, Client) :: nullable, notNullable)
                             | L.CApp ((L.CFfi ("Basis", "channel"), _), _) =>
                               (nullable, (x, Channel) :: notNullable)
                             | L.CApp ((L.CFfi ("Basis", "option"), _),
                                       (L.CApp ((L.CFfi ("Basis", "channel"), _), _), _)) =>
                               ((x, Channel) :: nullable, notNullable)
                             | _ => st)
                        | _ => st) ([], []) xts

        fun expunger () =
            let
                val target = (L'.EFfiApp ("Basis", "sqlifyClient", [((L'.ERel 0, loc), (L'.TFfi ("Basis", "client"), loc))]), loc)

                fun doTable (tab, xts, e) =
                    case xts of
                        L.CRecord (_, xts) =>
                        let
                            val (nullable, notNullable) = calcClientish xts

                            fun cond (x, v) =
                                (L'.EStrcat ((L'.EStrcat (str ("(("
                                                               ^ Settings.mangleSql x
                                                               ^ (case v of
                                                                      Client => ""
                                                                    | Channel => " >> 32")
                                                               ^ ") = "),
                                                          target), loc),
                                             str ")"), loc)

                            val e =
                                foldl (fn ((x, v), e) =>
                                          (L'.ESeq (
                                           (L'.EDml ((L'.EStrcat (
                                                      str ("UPDATE "
                                                           ^ Settings.mangleSql tab
                                                           ^ " SET "
                                                           ^ Settings.mangleSql x
                                                           ^ " = NULL WHERE "),
                                                      cond (x, v)), loc), L'.Error), loc),
                                           e), loc))
                                      e nullable

                            val e =
                                case notNullable of
                                    [] => e
                                  | eb :: ebs =>
                                    (L'.ESeq (
                                     (L'.EDml ((L'.EStrcat (str ("DELETE FROM "
                                                                 ^ Settings.mangleSql tab
                                                                 ^ " WHERE "),
                                                            foldl (fn (eb, s) =>
                                                                      (L'.EStrcat (str "(",
                                                                       (L'.EStrcat (s,
                                                                        (L'.EStrcat (str " OR ",
                                                                         (L'.EStrcat (cond eb,
                                                                                      str ")"),
                                                                          loc)), loc)), loc)), loc))
                                                                  (cond eb)
                                                                  ebs), loc),
                                      L'.Error), loc),
                                     e), loc)
                        in
                            e
                        end
                      | _ => e

                val e = (L'.ERecord [], loc)
            in
                foldl (fn ((d, _), e) =>
                          case d of
                              L.DTable (_, _, xts, tab, _, _, _, _) => doTable (tab, #1 xts, e)
                            | _ => e) e file
            end

        fun initializer () =
            let
                fun doTable (tab, xts, e) =
                    case xts of
                        L.CRecord (_, xts) =>
                        let
                            val (nullable, notNullable) = calcClientish xts

                            val e =
                                case nullable of
                                    [] => e
                                  | (x, _) :: ebs =>
                                    (L'.ESeq (
                                     (L'.EDml (str
                                                   (foldl (fn ((x, _), s) =>
                                                              s ^ ", " ^ Settings.mangleSql x ^ " = NULL")
                                                          ("UPDATE "
                                                           ^ Settings.mangleSql tab
                                                           ^ " SET "
                                                           ^ Settings.mangleSql x
                                                                      ^ " = NULL")
                                                          ebs), L'.Error), loc),
                                     e), loc)

                            val e =
                                case notNullable of
                                    [] => e
                                  | eb :: ebs =>
                                    (L'.ESeq (
                                     (L'.EDml (str ("DELETE FROM "
                                                    ^ Settings.mangleSql tab), L'.Error), loc),
                                     e), loc)
                        in
                            e
                        end
                      | _ => e

                val e = (L'.ERecord [], loc)
            in
                foldl (fn ((d, _), e) =>
                          case d of
                              L.DTable (_, _, xts, tab, _, _, _, _) => doTable (tab, #1 xts, e)
                            | _ => e) e file
            end

        val mname = CoreUtil.File.maxName file + 1
        val () = nextPvar := mname

        val (_, fm, ds) = List.foldl (fn (d, (env, fm, ds)) =>
                                        case #1 d of
                                            L.DDatabase s =>
                                            let
                                                val (nExp, fm) = Fm.freshName fm
                                                val (nIni, fm) = Fm.freshName fm

                                                val dExp = L'.DVal ("expunger",
                                                                    nExp,
                                                                    (L'.TFun (client, unit), loc),
                                                                    (L'.EAbs ("cli", client, unit, expunger ()), loc),
                                                                    "expunger")
                                                val dIni = L'.DVal ("initializer",
                                                                    nIni,
                                                                    (L'.TFun (unit, unit), loc),
                                                                    (L'.EAbs ("_", unit, unit, initializer ()), loc),
                                                                    "initializer")
                                            in
                                                (env, Fm.enter fm, (L'.DDatabase {name = s,
                                                                                  expunge = nExp,
                                                                                  initialize = nIni,
                                                                                  usesSimilar = false}, loc)
                                                                   :: (dExp, loc)
                                                                   :: (dIni, loc)
                                                                   :: ds)
                                            end
                                          | _ =>
                                            (pvarDefs := [];
                                             pvarOldDefs := [];
                                             case monoDecl (env, fm) d of
                                                 NONE => (env, fm, ds)
                                               | SOME (env, fm, ds') =>
                                                 (foldr (fn ((n, cs), env) =>
                                                            Env.declBinds env (L.DDatatype [("$poly" ^ Int.toString n,
                                                                                             n,
                                                                                             [],
                                                                                             cs)], loc))
                                                        env (!pvarOldDefs),
                                                  Fm.enter fm,
                                                  case ds' of
                                                      [(L'.DDatatype dts, loc)] =>
                                                      (L'.DDatatype (dts @ !pvarDefs), loc) :: Fm.decls fm @ ds
                                                    | _ =>
                                                      ds' @ Fm.decls fm @ (L'.DDatatype (!pvarDefs), loc) :: ds)))
                                     (env, Fm.empty mname, []) file
        val ds = map (fn (L'.DDatabase r, loc) =>
                         (L'.DDatabase {name = #name r,
                                        expunge = #expunge r,
                                        initialize = #initialize r,
                                        usesSimilar = !uses_similar}, loc)
                     | x => x) ds
        val monoFile = (rev ds, [])
    in
        pvars := RM.empty;
        pvarDefs := [];
        pvarOldDefs := [];
        MonoFooify.canonicalFm := Fm.empty (MonoUtil.File.maxName monoFile + 1);
        monoFile
    end

end
