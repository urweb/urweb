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

structure Monoize :> MONOIZE = struct

structure E = ErrorMsg
structure Env = CoreEnv

structure L = Core
structure L' = Mono

structure IM = IntBinaryMap

val urlPrefix = ref "/"

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
                    (L'.TRecord (map (fn (x, t) => (monoName env x, mt env dtmap t)) xcs), loc)
                  | L.TRecord _ => poly ()

                  | L.CApp ((L.CFfi ("Basis", "option"), _), t) =>
                    (L'.TOption (mt env dtmap t), loc)

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
                                     ("Mod", (L'.TFun (t, (L'.TFun (t, t), loc)), loc))],
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

                  | L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "xml"), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "xhtml"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)

                  | L.CApp ((L.CFfi ("Basis", "transaction"), _), t) =>
                    (L'.TFun ((L'.TRecord [], loc), mt env dtmap t), loc)
                  | L.CApp ((L.CFfi ("Basis", "source"), _), t) =>
                    (L'.TFfi ("Basis", "int"), loc)
                  | L.CApp ((L.CFfi ("Basis", "signal"), _), t) =>
                    (L'.TSignal (mt env dtmap t), loc)
                  | L.CApp ((L.CFfi ("Basis", "http_cookie"), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_table"), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CFfi ("Basis", "sql_sequence") =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_query"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_query1"), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_exp"), _), _), _), _), _), _), _), _) =>
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

                  | L.CApp ((L.CFfi ("Basis", "sql_injectable"), _), t) =>
                    (L'.TFun (mt env dtmap t, (L'.TFfi ("Basis", "string"), loc)), loc)
                  | L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_unary"), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "sql_binary"), _), _), _), _), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_aggregate"), _), t) =>
                    (L'.TFfi ("Basis", "string"), loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_summable"), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_maxable"), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_arith"), _), _) =>
                    (L'.TRecord [], loc)
                  | L.CApp ((L.CFfi ("Basis", "sql_nfunc"), _), _) =>
                    (L'.TFfi ("Basis", "string"), loc)

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
                  | L.CFold _ => poly ()
                  | L.CUnit => poly ()

                  | L.CTuple _ => poly ()
                  | L.CProj _ => poly ()
            end
    in
        mt env IM.empty
    end

val dummyExp = (L'.EPrim (Prim.Int 0), E.dummySpan)

structure IM = IntBinaryMap

datatype foo_kind =
         Attr
       | Url

fun fk2s fk =
    case fk of
        Attr => "attr"
      | Url => "url"

structure Fm :> sig
    type t

    val empty : int -> t

    val lookup : t -> foo_kind -> int -> (int -> t -> L'.decl * t) -> t * int
    val enter : t -> t
    val decls : t -> L'.decl list
end = struct

structure M = BinaryMapFn(struct
                          type ord_key = foo_kind
                          fun compare x =
                              case x of
                                  (Attr, Attr) => EQUAL
                                | (Attr, _) => LESS
                                | (_, Attr) => GREATER

                                | (Url, Url) => EQUAL
                          end)

type t = {
     count : int,
     map : int IM.map M.map,
     decls : L'.decl list
}

fun empty count = {
    count = count,
    map = M.empty,
    decls = []
}

fun enter ({count, map, ...} : t) = {count = count, map = map, decls = []}
fun decls ({decls, ...} : t) = decls

fun lookup (t as {count, map, decls}) k n thunk =
    let
        val im = Option.getOpt (M.find (map, k), IM.empty)
    in
        case IM.find (im, n) of
            NONE =>
            let
                val n' = count
                val (d, {count, map, decls}) = thunk count {count = count + 1,
                                                            map = M.insert (map, k, IM.insert (im, n, n')),
                                                            decls = decls}
            in
                ({count = count,
                  map = map,
                  decls = d :: decls}, n')
            end
          | SOME n' => (t, n')
    end

end


fun capitalize s =
    if s = "" then
        s
    else
        str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

fun fooifyExp fk env =
    let
        fun fooify fm (e, tAll as (t, loc)) =
            case #1 e of
                L'.EClosure (fnam, [(L'.ERecord [], _)]) =>
                let
                    val (_, _, _, s) = Env.lookupENamed env fnam
                in
                    ((L'.EPrim (Prim.String (!urlPrefix ^ s)), loc), fm)
                end
              | L'.EClosure (fnam, args) =>
                let
                    val (_, ft, _, s) = Env.lookupENamed env fnam
                    val ft = monoType env ft

                    fun attrify (args, ft, e, fm) =
                        case (args, ft) of
                            ([], _) => (e, fm)
                          | (arg :: args, (L'.TFun (t, ft), _)) =>
                            let
                                val (arg', fm) = fooify fm (arg, t)
                            in
                                attrify (args, ft,
                                         (L'.EStrcat (e,
                                                      (L'.EStrcat ((L'.EPrim (Prim.String "/"), loc),
                                                                   arg'), loc)), loc),
                                         fm)
                            end
                          | _ => (E.errorAt loc "Type mismatch encoding attribute";
                                  (e, fm))
                in
                    attrify (args, ft, (L'.EPrim (Prim.String (!urlPrefix ^ s)), loc), fm)
                end
              | _ =>
                case t of
                    L'.TFfi (m, x) => ((L'.EFfiApp (m, fk2s fk ^ "ify" ^ capitalize x, [e]), loc), fm)

                  | L'.TRecord [] => ((L'.EPrim (Prim.String ""), loc), fm)
                  | L'.TRecord ((x, t) :: xts) =>
                    let
                        val (se, fm) = fooify fm ((L'.EField (e, x), loc), t)
                    in
                        foldl (fn ((x, t), (se, fm)) =>
                                  let
                                      val (se', fm) = fooify fm ((L'.EField (e, x), loc), t)
                                  in
                                      ((L'.EStrcat (se,
                                                    (L'.EStrcat ((L'.EPrim (Prim.String "/"), loc),
                                                                 se'), loc)), loc),
                                       fm)
                                  end) (se, fm) xts
                    end

                  | L'.TDatatype (i, ref (dk, _)) =>
                    let
                        fun makeDecl n fm =
                            let
                                val (x, _, xncs) = Env.lookupDatatype env i

                                val (branches, fm) =
                                    ListUtil.foldlMap
                                        (fn ((x, n, to), fm) =>
                                            case to of
                                                NONE =>
                                                (((L'.PCon (dk, L'.PConVar n, NONE), loc),
                                                  (L'.EPrim (Prim.String x), loc)),
                                                 fm)
                                              | SOME t =>
                                                let
                                                    val t = monoType env t
                                                    val (arg, fm) = fooify fm ((L'.ERel 0, loc), t)
                                                in
                                                    (((L'.PCon (dk, L'.PConVar n, SOME (L'.PVar ("a", t), loc)), loc),
                                                      (L'.EStrcat ((L'.EPrim (Prim.String (x ^ "/")), loc),
                                                                   arg), loc)),
                                                     fm)
                                                end)
                                        fm xncs

                                val dom = tAll
                                val ran = (L'.TFfi ("Basis", "string"), loc)
                            in
                                ((L'.DValRec [(fk2s fk ^ "ify_" ^ x,
                                               n,
                                               (L'.TFun (dom, ran), loc),
                                               (L'.EAbs ("x",
                                                         dom,
                                                         ran,
                                                         (L'.ECase ((L'.ERel 0, loc),
                                                                    branches,
                                                                    {disc = dom,
                                                                     result = ran}), loc)), loc),
                                               "")], loc),
                                 fm)
                            end       

                        val (fm, n) = Fm.lookup fm fk i makeDecl
                    in
                        ((L'.EApp ((L'.ENamed n, loc), e), loc), fm)
                    end

                  | L'.TOption t =>
                    let
                        val (body, fm) = fooify fm ((L'.ERel 0, loc), t)
                    in
                        ((L'.ECase (e,
                                    [((L'.PNone t, loc),
                                      (L'.EPrim (Prim.String "None"), loc)),
                                     
                                     ((L'.PSome (t, (L'.PVar ("x", t), loc)), loc),
                                      (L'.EStrcat ((L'.EPrim (Prim.String "Some/"), loc),
                                                   body), loc))],
                                    {disc = tAll,
                                     result = (L'.TFfi ("Basis", "string"), loc)}), loc),
                         fm)
                    end

                  | _ => (E.errorAt loc "Don't know how to encode attribute/URL type";
                          Print.eprefaces' [("Type", MonoPrint.p_typ MonoEnv.empty tAll)];
                          (dummyExp, fm))
    in
        fooify
    end

val attrifyExp = fooifyExp Attr
val urlifyExp = fooifyExp Url

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

fun monoPat env (all as (p, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported pattern";
             Print.eprefaces' [("Pattern", CorePrint.p_pat env all)];
             dummyPat)
    in
        case p of
            L.PWild => (L'.PWild, loc)
          | L.PVar (x, t) => (L'.PVar (x, monoType env t), loc)
          | L.PPrim p => (L'.PPrim p, loc)
          | L.PCon (dk, pc, [], po) => (L'.PCon (dk, monoPatCon env pc, Option.map (monoPat env) po), loc)
          | L.PCon (L.Option, _, [t], NONE) => (L'.PNone (monoType env t), loc)
          | L.PCon (L.Option, _, [t], SOME p) => (L'.PSome (monoType env t, monoPat env p), loc)
          | L.PCon _ => poly ()
          | L.PRecord xps => (L'.PRecord (map (fn (x, p, t) => (x, monoPat env p, monoType env t)) xps), loc)
    end

fun strcat loc es =
    case es of
        [] => (L'.EPrim (Prim.String ""), loc)
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
        [] => (L'.EPrim (Prim.String ""), loc)
      | [e] => e
      | _ =>
        let
            val e1 = List.last es
            val es = List.take (es, length es - 1)
        in
            foldr (fn (e, e') =>
                      case (e, e') of
                          ((L'.EPrim (Prim.String ""), _), _) => e'
                        | (_, (L'.EPrim (Prim.String ""), _)) => e
                        | _ =>
                          (L'.EStrcat (e,
                                       (L'.EStrcat ((L'.EPrim (Prim.String ", "), loc), e'), loc)), loc))
            e1 es
        end

fun strcatR loc e xs = strcatComma loc (map (fn (x, _) => (L'.EField (e, x), loc)) xs)

fun monoExp (env, st, fm) (all as (e, loc)) =
    let
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
                         ("Mod", (L'.TFun (t, (L'.TFun (t, t), loc)), loc))], loc)
        fun numEx (t, zero, neg, plus, minus, times, dv, md) =
            ((L'.ERecord [("Zero", (L'.EPrim zero, loc), t),
                          ("Neg", neg, (L'.TFun (t, t), loc)),
                          ("Plus", plus, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Minus", minus, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Times", times, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Div", dv, (L'.TFun (t, (L'.TFun (t, t), loc)), loc)),
                          ("Mod", md, (L'.TFun (t, (L'.TFun (t, t), loc)), loc))], loc), fm)

        fun ordTy t =
            (L'.TRecord [("Lt", (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc)),
                         ("Le", (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc))], loc)
        fun ordEx (t, lt, le) =
            ((L'.ERecord [("Lt", lt, (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc)),
                          ("Le", le, (L'.TFun (t, (L'.TFun (t, (L'.TFfi ("Basis", "bool"), loc)), loc)), loc))],
              loc), fm)
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
          | L.ECon (L.Option, _, [t], NONE) =>
            ((L'.ENone (monoType env t), loc), fm)
          | L.ECon (L.Option, _, [t], SOME e) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.ESome (monoType env t, e), loc), fm)
            end
          | L.ECon _ => poly ()

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
                                 (L'.EBinop ("==", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_float") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "float"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "float"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop ("==", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_bool") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "bool"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "bool"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop ("==", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_string") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "string"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "string"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "string"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop ("!strcmp", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
             fm)
          | L.EFfi ("Basis", "eq_time") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "time"), loc),
                       (L'.TFun ((L'.TFfi ("Basis", "time"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                       (L'.EAbs ("y", (L'.TFfi ("Basis", "time"), loc),
                                 (L'.TFfi ("Basis", "bool"), loc),
                                 (L'.EBinop ("==", (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc),
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
          | L.ECApp ((L.EFfi ("Basis", "div"), _), t) =>
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
          | L.EFfi ("Basis", "num_int") =>
            let
                fun intBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "int"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "int"), loc), (L'.TFfi ("Basis", "int"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "int"), loc),
                                        (L'.TFfi ("Basis", "int"), loc),
                                        (L'.EBinop (s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
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
                       intBin "%")
            end
          | L.EFfi ("Basis", "num_float") =>
            let
                fun floatBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "float"), loc), (L'.TFfi ("Basis", "float"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "float"), loc),
                                        (L'.TFfi ("Basis", "float"), loc),
                                        (L'.EBinop (s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                numEx ((L'.TFfi ("Basis", "float"), loc),
                       Prim.Float 0.0,
                       (L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc),
                                 (L'.TFfi ("Basis", "float"), loc),
                                 (L'.EUnop ("-", (L'.ERel 0, loc)), loc)), loc),
                       floatBin "+",
                       floatBin "-",
                       floatBin "*",
                       floatBin "/",
                       floatBin "fmod")
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
                                        (L'.EBinop (s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
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
                                        (L'.EBinop (s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
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
                                        (L'.EBinop (s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
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
                                        (L'.EBinop (s,
                                                    (L'.EBinop ("strcmp",
                                                                (L'.ERel 1, loc),
                                                                (L'.ERel 0, loc)), loc),
                                                    (L'.EPrim (Prim.Int (Int64.fromInt 0)), loc)), loc)), loc)), loc)
            in
                ordEx ((L'.TFfi ("Basis", "string"), loc),
                       boolBin "<",
                       boolBin "<=")
            end
          | L.EFfi ("Basis", "ord_time") =>
            let
                fun boolBin s =
                    (L'.EAbs ("x", (L'.TFfi ("Basis", "time"), loc),
                              (L'.TFun ((L'.TFfi ("Basis", "time"), loc), (L'.TFfi ("Basis", "bool"), loc)), loc),
                              (L'.EAbs ("y", (L'.TFfi ("Basis", "time"), loc),
                                        (L'.TFfi ("Basis", "bool"), loc),
                                        (L'.EBinop (s, (L'.ERel 1, loc), (L'.ERel 0, loc)), loc)), loc)), loc)
            in
                ordEx ((L'.TFfi ("Basis", "time"), loc),
                       boolBin "<",
                       boolBin "<=")
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
          | L.EFfi ("Basis", "show_bool") =>
            ((L'.EFfi ("Basis", "boolToString"), loc), fm)
          | L.EFfi ("Basis", "show_time") =>
            ((L'.EFfi ("Basis", "timeToString"), loc), fm)
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

          | L.EApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "return"), _), _), _), t), _),
                    (L.EFfi ("Basis", "transaction_monad"), _)) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("x", t,
                           (L'.TFun ((L'.TRecord [], loc), t), loc),
                           (L'.EAbs ("_", (L'.TRecord [], loc), t,
                                     (L'.ERel 1, loc)), loc)), loc), fm)
            end
          | L.EApp ((L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "bind"), _), _), _), t1), _), t2), _),
                    (L.EFfi ("Basis", "transaction_monad"), _)) =>
            let
                val t1 = monoType env t1
                val t2 = monoType env t2
                val un = (L'.TRecord [], loc)
                val mt1 = (L'.TFun (un, t1), loc)
                val mt2 = (L'.TFun (un, t2), loc)
            in
                ((L'.EAbs ("m1", mt1, (L'.TFun (mt1, (L'.TFun (mt2, (L'.TFun (un, un), loc)), loc)), loc),
                           (L'.EAbs ("m2", mt2, (L'.TFun (un, un), loc),
                                     (L'.EAbs ("_", un, un,
                                               (L'.ELet ("r", t1, (L'.EApp ((L'.ERel 2, loc),
                                                                            (L'.ERecord [], loc)), loc),
                                                         (L'.EApp (
                                                          (L'.EApp ((L'.ERel 2, loc), (L'.ERel 0, loc)), loc),
                                                          (L'.ERecord [], loc)),
                                                          loc)), loc)), loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "source"), _), t) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("x", t, (L'.TFun ((L'.TRecord [], loc), (L'.TFfi ("Basis", "int"), loc)), loc),
                           (L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "int"), loc),
                                     (L'.EFfiApp ("Basis", "new_client_source", [(L'.ERecord [], loc)]), loc)), loc)),
                  loc),
                 fm)
            end

          | L.EApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "return"), _), _), _), t), _),
                    (L.EFfi ("Basis", "signal_monad"), _)) =>
            let
                val t = monoType env t
            in
                ((L'.EAbs ("x", t, (L'.TSignal t, loc),
                           (L'.ESignalReturn (L'.ERel 0, loc), loc)), loc),
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
                                     (L'.EUnurlify ((L'.EFfiApp ("Basis", "get_cookie", [(L'.ERel 1, loc)]), loc),
                                                    t),
                                      loc)), loc)), loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "setCookie"), _), t) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                val un = (L'.TRecord [], loc)
                val t = monoType env t
                val (e, fm) = urlifyExp env fm ((L'.ERel 1, loc), t)
            in
                ((L'.EAbs ("c", s, (L'.TFun (t, (L'.TFun (un, un), loc)), loc),
                           (L'.EAbs ("v", t, (L'.TFun (un, un), loc),
                                     (L'.EAbs ("_", un, un,
                                               (L'.EFfiApp ("Basis", "set_cookie", [(L'.EPrim (Prim.String (!urlPrefix)),
                                                                                     loc),
                                                                                    (L'.ERel 2, loc),
                                                                                    e]), loc)),
                                      loc)), loc)), loc),
                 fm)
            end            

          | L.EFfiApp ("Basis", "dml", [e]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.EDml e, loc),
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "insert"), _), fields) =>
            (case monoType env (L.TRecord fields, loc) of
                 (L'.TRecord fields, _) =>
                 let
                     val s = (L'.TFfi ("Basis", "string"), loc)
                     val fields = map (fn (x, _) => (x, s)) fields
                     val rt = (L'.TRecord fields, loc)
                     fun sc s = (L'.EPrim (Prim.String s), loc)
                 in
                     ((L'.EAbs ("tab", s, (L'.TFun (rt, s), loc),
                                (L'.EAbs ("fs", rt, s,
                                          strcat loc [sc "INSERT INTO ",
                                                      (L'.ERel 1, loc),
                                                      sc " (",
                                                      strcatComma loc (map (fn (x, _) => sc ("uw_" ^ x)) fields),
                                                      sc ") VALUES (",
                                                      strcatComma loc (map (fn (x, _) =>
                                                                               (L'.EField ((L'.ERel 0, loc),
                                                                                           x), loc)) fields),
                                                      sc ")"]), loc)), loc),
                      fm)
                 end
               | _ => poly ())

          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "update"), _), _), _), changed) =>
            (case monoType env (L.TRecord changed, loc) of
                 (L'.TRecord changed, _) =>
                 let
                     val s = (L'.TFfi ("Basis", "string"), loc)
                     val changed = map (fn (x, _) => (x, s)) changed
                     val rt = (L'.TRecord changed, loc)
                     fun sc s = (L'.EPrim (Prim.String s), loc)
                 in
                     ((L'.EAbs ("fs", rt, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                                (L'.EAbs ("tab", s, (L'.TFun (s, s), loc),
                                          (L'.EAbs ("e", s, s,
                                                    strcat loc [sc "UPDATE ",
                                                                (L'.ERel 1, loc),
                                                                sc " AS T SET ",
                                                                strcatComma loc (map (fn (x, _) =>
                                                                                         strcat loc [sc ("uw_" ^ x
                                                                                                         ^ " = "),
                                                                                                     (L'.EField
                                                                                                          ((L'.ERel 2,
                                                                                                            loc),
                                                                                                           x), loc)])
                                                                                     changed),
                                                                sc " WHERE ",
                                                                (L'.ERel 0, loc)]), loc)), loc)), loc),
                      fm)
                 end
               | _ => poly ())

          | L.ECApp ((L.EFfi ("Basis", "delete"), _), _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                fun sc s = (L'.EPrim (Prim.String s), loc)
            in
                ((L'.EAbs ("tab", s, (L'.TFun (s, s), loc),
                           (L'.EAbs ("e", s, s,
                                     strcat loc [sc "DELETE FROM ",
                                                 (L'.ERel 1, loc),
                                                 sc " AS T WHERE ",
                                                 (L'.ERel 0, loc)]), loc)), loc),
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

          | L.ECApp ((L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_query"), _), _), _), _), _), _) =>
            let
                fun sc s = (L'.EPrim (Prim.String s), loc)
                val s = (L'.TFfi ("Basis", "string"), loc)
                fun gf s = (L'.EField ((L'.ERel 0, loc), s), loc)
            in
                ((L'.EAbs ("r",
                           (L'.TRecord [("Rows", s), ("OrderBy", s), ("Limit", s), ("Offset", s)], loc),
                           s,
                           strcat loc [gf "Rows",
                                       (L'.ECase (gf "OrderBy",
                                                  [((L'.PPrim (Prim.String ""), loc), sc ""),
                                                   ((L'.PWild, loc),
                                                    strcat loc [sc " ORDER BY ",
                                                                gf "OrderBy"])],
                                                  {disc = s, result = s}), loc),
                                       gf "Limit",
                                       gf "Offset"]), loc), fm)
            end

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_query1"), _),
               (L.CRecord (_, tables), _)), _),
              (L.CRecord (_, grouped), _)), _),
             (L.CRecord (_, stables), _)), _),
            sexps) =>
            let
                fun sc s = (L'.EPrim (Prim.String s), loc)
                val s = (L'.TFfi ("Basis", "string"), loc)
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
                                   (L'.TRecord [("From", (L'.TRecord (map (fn (x, _) => (x, s)) tables), loc)),
                                                ("Where", s),
                                                ("GroupBy", un),
                                                ("Having", s),
                                                ("SelectFields", un),
                                                ("SelectExps", (L'.TRecord (map (fn (x, _) => (x, s)) sexps), loc))],
                                    loc),
                                   s,
                                   strcat loc [sc "SELECT ",
                                               strcatComma loc (map (fn (x, t) =>
                                                                        strcat loc [
                                                                        (L'.EField (gf "SelectExps", x), loc),
                                                                        sc (" AS _" ^ x)
                                                                    ]) sexps
                                                                @ map (fn (x, xts) =>
                                                                          strcatComma loc
                                                                                      (map (fn (x', _) =>
                                                                                               sc (x ^ ".uw_" ^ x'))
                                                                                           xts)) stables),
                                               sc " FROM ",
                                               strcatComma loc (map (fn (x, _) => strcat loc [(L'.EField (gf "From", x), loc),
                                                                                              sc (" AS " ^ x)]) tables),
                                               (L'.ECase (gf "Where",
                                                          [((L'.PPrim (Prim.String "TRUE"), loc),
                                                            sc ""),
                                                           ((L'.PWild, loc),
                                                            strcat loc [sc " WHERE ", gf "Where"])],
                                                          {disc = s,
                                                           result = s}), loc),
                                               
                                               if List.all (fn (x, xts) =>
                                                               case List.find (fn (x', _) => x' = x) grouped of
                                                                   NONE => List.null xts
                                                                 | SOME (_, xts') =>
                                                                   List.all (fn (x, _) =>
                                                                                List.exists (fn (x', _) => x' = x)
                                                                                            xts') xts) tables then
                                                   sc ""
                                               else
                                                   strcat loc [
                                                   sc " GROUP BY ",
                                                   strcatComma loc (map (fn (x, xts) =>
                                                                            strcatComma loc
                                                                                        (map (fn (x', _) =>
                                                                                                 sc (x ^ ".uw_" ^ x'))
                                                                                             xts)) grouped)
                                                   ],

                                               (L'.ECase (gf "Having",
                                                          [((L'.PPrim (Prim.String "TRUE"), loc),
                                                            sc ""),
                                                           ((L'.PWild, loc),
                                                            strcat loc [sc " HAVING ", gf "Having"])],
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
                       (L'.EFfiApp ("Basis", "sqlifyInt", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_float") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "float"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyFloat", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_bool") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "bool"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyBool", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_string") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "string"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyString", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_time") =>
            ((L'.EAbs ("x", (L'.TFfi ("Basis", "time"), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyTime", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_option_int") =>
            ((L'.EAbs ("x", (L'.TOption (L'.TFfi ("Basis", "int"), loc), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyIntN", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_option_float") =>
            ((L'.EAbs ("x", (L'.TOption (L'.TFfi ("Basis", "float"), loc), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyFloatN", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_option_bool") =>
            ((L'.EAbs ("x", (L'.TOption (L'.TFfi ("Basis", "bool"), loc), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyBoolN", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_option_string") =>
            ((L'.EAbs ("x", (L'.TOption (L'.TFfi ("Basis", "string"), loc), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyStringN", [(L'.ERel 0, loc)]), loc)), loc),
             fm)
          | L.EFfi ("Basis", "sql_option_time") =>
            ((L'.EAbs ("x", (L'.TOption (L'.TFfi ("Basis", "time"), loc), loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EFfiApp ("Basis", "sqlifyTimeN", [(L'.ERel 0, loc)]), loc)), loc),
             fm)

          | L.ECApp ((L.EFfi ("Basis", "sql_subset"), _), _) =>
            ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_subset_all"), _), _) =>
            ((L'.ERecord [], loc), fm)

          | L.ECApp ((L.ECApp ((L.EFfi ("Basis", "sql_order_by_Nil"), _), _), _), _) =>
            ((L'.EPrim (Prim.String ""), loc), fm)
          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.EFfi ("Basis", "sql_order_by_Cons"), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                fun sc s = (L'.EPrim (Prim.String s), loc)
            in
                ((L'.EAbs ("e1", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("d", s, (L'.TFun (s, s), loc),
                                     (L'.EAbs ("e2", s, s,
                                               (L'.ECase ((L'.ERel 0, loc),
                                                          [((L'.PPrim (Prim.String ""), loc),
                                                            strcat loc [(L'.ERel 2, loc),
                                                                        (L'.ERel 1, loc)]),
                                                           ((L'.PWild, loc),
                                                            strcat loc [(L'.ERel 2, loc),
                                                                        (L'.ERel 1, loc),
                                                                        sc ", ",
                                                                        (L'.ERel 0, loc)])],
                                                          {disc = s, result = s}), loc)), loc)), loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "sql_no_limit") =>
            ((L'.EPrim (Prim.String ""), loc), fm)
          | L.EFfiApp ("Basis", "sql_limit", [e]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                (strcat loc [
                 (L'.EPrim (Prim.String " LIMIT "), loc),
                 (L'.EFfiApp ("Basis", "sqlifyInt", [e]), loc)
                 ],
                 fm)
            end

          | L.EFfi ("Basis", "sql_no_offset") =>
            ((L'.EPrim (Prim.String ""), loc), fm)
          | L.EFfiApp ("Basis", "sql_offset", [e]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                (strcat loc [
                 (L'.EPrim (Prim.String " OFFSET "), loc),
                 (L'.EFfiApp ("Basis", "sqlifyInt", [e]), loc)
                 ],
                 fm)
            end

          | L.ECApp ((L.EFfi ("Basis", "sql_eq"), _), _) =>
            ((L'.EPrim (Prim.String "="), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_ne"), _), _) =>
            ((L'.EPrim (Prim.String "<>"), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_lt"), _), _) =>
            ((L'.EPrim (Prim.String "<"), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_le"), _), _) =>
            ((L'.EPrim (Prim.String "<="), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_gt"), _), _) =>
            ((L'.EPrim (Prim.String ">"), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_ge"), _), _) =>
            ((L'.EPrim (Prim.String ">="), loc), fm)

          | L.ECApp ((L.EFfi ("Basis", "sql_plus"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "+"), loc)), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_minus"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "-"), loc)), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_times"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "*"), loc)), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_div"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "/"), loc)), loc), fm)
          | L.EFfi ("Basis", "sql_mod") =>
            ((L'.EPrim (Prim.String "%"), loc), fm)

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
                fun sc s = (L'.EPrim (Prim.String s), loc)
            in
                ((L'.EAbs ("c", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("e1", s, (L'.TFun (s, s), loc),
                                     strcat loc [sc "(",
                                                 (L'.ERel 1, loc),
                                                 sc " ",
                                                 (L'.ERel 0, loc),
                                                 sc ")"]), loc)), loc),
                 fm)
            end
          | L.EFfi ("Basis", "sql_not") => ((L'.EPrim (Prim.String "NOT"), loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_neg"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "-"), loc)), loc), fm)

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
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                fun sc s = (L'.EPrim (Prim.String s), loc)
            in
                ((L'.EAbs ("c", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("e1", s, (L'.TFun (s, s), loc),
                                     (L'.EAbs ("e2", s, s,
                                               strcat loc [sc "(",
                                                           (L'.ERel 1, loc),
                                                           sc " ",
                                                           (L'.ERel 2, loc),
                                                           sc " ",
                                                           (L'.ERel 0, loc),
                                                           sc ")"]), loc)), loc)), loc),
                 fm)
            end
          | L.EFfi ("Basis", "sql_and") => ((L'.EPrim (Prim.String "AND"), loc), fm)
          | L.EFfi ("Basis", "sql_or") => ((L'.EPrim (Prim.String "OR"), loc), fm)

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
            (L.CName field, _)) => ((L'.EPrim (Prim.String (tab ^ ".uw_" ^ field)), loc), fm)

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
            (L.CName nm, _)) => ((L'.EPrim (Prim.String ("_" ^ nm)), loc), fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_relop"), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                fun sc s = (L'.EPrim (Prim.String s), loc)
            in
                ((L'.EAbs ("c", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("e1", s, (L'.TFun (s, s), loc),
                                     (L'.EAbs ("e2", s, s,
                                               strcat loc [sc "((",
                                                           (L'.ERel 1, loc),
                                                           sc ") ",
                                                           (L'.ERel 2, loc),
                                                           sc " (",
                                                           (L'.ERel 0, loc),
                                                           sc "))"]), loc)), loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "sql_union") => ((L'.EPrim (Prim.String "UNION"), loc), fm)
          | L.EFfi ("Basis", "sql_intersect") => ((L'.EPrim (Prim.String "INTERSECT"), loc), fm)
          | L.EFfi ("Basis", "sql_except") => ((L'.EPrim (Prim.String "EXCEPT"), loc), fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.EFfi ("Basis", "sql_count"), _),
              _), _),
             _), _),
            _) => ((L'.EPrim (Prim.String "COUNT(*)"), loc),
                   fm)

          | L.ECApp (
            (L.ECApp (
             (L.ECApp (
              (L.ECApp (
               (L.EFfi ("Basis", "sql_aggregate"), _),
               _), _),
              _), _),
             _), _),
            _) =>
            let
                val s = (L'.TFfi ("Basis", "string"), loc)
                fun sc s = (L'.EPrim (Prim.String s), loc)
            in
                ((L'.EAbs ("c", s, (L'.TFun (s, (L'.TFun (s, s), loc)), loc),
                           (L'.EAbs ("e1", s, (L'.TFun (s, s), loc),
                                     strcat loc [(L'.ERel 1, loc),
                                                 sc "(",
                                                 (L'.ERel 0, loc),
                                                 sc ")"]), loc)), loc),
                 fm)
            end

          | L.EFfi ("Basis", "sql_summable_int") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_summable_float") => ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_avg"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "AVG"), loc)), loc),
             fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_sum"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "SUM"), loc)), loc),
             fm)

          | L.EFfi ("Basis", "sql_arith_int") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_arith_float") => ((L'.ERecord [], loc), fm)

          | L.EFfi ("Basis", "sql_maxable_int") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_maxable_float") => ((L'.ERecord [], loc), fm)
          | L.EFfi ("Basis", "sql_maxable_string") => ((L'.ERecord [], loc), fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_max"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "MAX"), loc)), loc),
             fm)
          | L.ECApp ((L.EFfi ("Basis", "sql_min"), _), _) =>
            ((L'.EAbs ("_", (L'.TRecord [], loc), (L'.TFfi ("Basis", "string"), loc),
                       (L'.EPrim (Prim.String "MIN"), loc)), loc),
             fm)

          | L.EFfi ("Basis", "sql_asc") => ((L'.EPrim (Prim.String ""), loc), fm)
          | L.EFfi ("Basis", "sql_desc") => ((L'.EPrim (Prim.String " DESC"), loc), fm)

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
                fun sc s = (L'.EPrim (Prim.String s), loc)
            in
                ((L'.EAbs ("s", s, s, (L'.ERel 0, loc)), loc),
                 fm)
            end
          | L.EFfi ("Basis", "sql_current_timestamp") => ((L'.EPrim (Prim.String "CURRENT_TIMESTAMP"), loc), fm)

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
                fun sc s = (L'.EPrim (Prim.String s), loc)
            in
                ((L'.EAbs ("s", s, s,
                           strcat loc [sc "(",
                                       (L'.ERel 0, loc),
                                       sc " IS NULL)"]), loc),
                 fm)
            end

          | L.EFfiApp ("Basis", "nextval", [e]) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.ENextval e, loc), fm)
            end

          | L.EApp (
            (L.ECApp (
             (L.ECApp ((L.EFfi ("Basis", "cdata"), _), _), _),
             _), _),
            se) =>
            let
                val (se, fm) = monoExp (env, st, fm) se
            in
                ((L'.EFfiApp ("Basis", "htmlifyString", [se]), loc), fm)
            end

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
              attrs), _),
             tag), _),
            xml) =>
            let
                fun getTag' (e, _) =
                    case e of
                        L.EFfi ("Basis", tag) => (tag, [])
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
                        L.EFfiApp ("Basis", tag, [(L.ERecord [], _)]) => (tag, [])
                      | L.EApp (e, (L.ERecord [], _)) => getTag' e
                      | _ => (E.errorAt loc "Non-constant XML tag";
                              Print.eprefaces' [("Expression", CorePrint.p_exp env tag)];
                              ("", []))

                val (tag, targs) = getTag tag

                val (attrs, fm) = monoExp (env, st, fm) attrs

                fun tagStart tag =
                    case #1 attrs of
                        L'.ERecord xes =>
                        let
                            fun lowercaseFirst "" = ""
                              | lowercaseFirst s = str (Char.toLower (String.sub (s, 0)))
                                                   ^ String.extract (s, 1, NONE)

                            val s = (L'.EPrim (Prim.String (String.concat ["<", tag])), loc)
                        in
                            foldl (fn (("Action", _, _), acc) => acc
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
                                                                         (L'.EPrim (Prim.String s'), loc)), loc)),
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
                                        | (L'.TFun _, _) =>
                                          let
                                              val s' = " " ^ lowercaseFirst x ^ "='"
                                          in
                                              ((L'.EStrcat (s,
                                                            (L'.EStrcat (
                                                             (L'.EPrim (Prim.String s'), loc),
                                                             (L'.EStrcat (
                                                              (L'.EJavaScript (L'.Attribute, e), loc),
                                                              (L'.EPrim (Prim.String "'"), loc)), loc)),
                                                             loc)), loc),
                                               fm)
                                          end
                                        | _ =>
                                          let
                                              val fooify =
                                                  case x of
                                                      "Href" => urlifyExp
                                                    | "Link" => urlifyExp
                                                    | _ => attrifyExp

                                              val xp = " " ^ lowercaseFirst x ^ "=\""

                                              val (e, fm) = fooify env fm (e, t)
                                          in
                                              ((L'.EStrcat (s,
                                                            (L'.EStrcat ((L'.EPrim (Prim.String xp), loc),
                                                                         (L'.EStrcat (e,
                                                                                      (L'.EPrim (Prim.String "\""),
                                                                                       loc)),
                                                                          loc)),
                                                             loc)), loc),
                                               fm)
                                          end)
                                  (s, fm) xes
                        end
                      | _ => raise Fail "Non-record attributes!"

                fun input typ =
                    case targs of
                        [_, (L.CName name, _)] =>
                        let
                            val (ts, fm) = tagStart "input"
                        in
                            ((L'.EStrcat (ts,
                                          (L'.EPrim (Prim.String (" type=\"" ^ typ ^ "\" name=\"" ^ name ^ "\"/>")),
                                           loc)), loc), fm)
                        end
                      | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                              raise Fail "No name passed to input tag")

                fun normal (tag, extra, extraInner) =
                    let
                        val (tagStart, fm) = tagStart tag
                        val tagStart = case extra of
                                           NONE => tagStart
                                         | SOME extra => (L'.EStrcat (tagStart, extra), loc)

                        fun normal () =
                            let
                                val (xml, fm) = monoExp (env, st, fm) xml
                                val xml = case extraInner of
                                              NONE => xml
                                            | SOME ei => (L'.EStrcat (ei, xml), loc)
                            in
                                ((L'.EStrcat ((L'.EStrcat (tagStart, (L'.EPrim (Prim.String ">"), loc)), loc),
                                              (L'.EStrcat (xml,
                                                           (L'.EPrim (Prim.String (String.concat ["</", tag, ">"])),
                                                            loc)), loc)),
                                  loc),
                                 fm)
                            end
                    in
                        case xml of
                            (L.EApp ((L.ECApp (
                                      (L.ECApp ((L.EFfi ("Basis", "cdata"), _),
                                                _), _),
                                      _), _),
                                     (L.EPrim (Prim.String s), _)), _) =>
                            if CharVector.all Char.isSpace s then
                                ((L'.EStrcat (tagStart, (L'.EPrim (Prim.String "/>"), loc)), loc), fm)
                            else
                                normal ()
                          | _ => normal ()
                    end
            in
                case tag of
                    "body" => normal ("body", NONE,
                                      SOME (L'.EStrcat ((L'.EPrim (Prim.String "<script src=\"/app.js\"></script>"), loc),
                                                        (L'.EFfiApp ("Basis", "get_script", [(L'.ERecord [], loc)]),
                                                         loc)), loc))

                  | "dyn" =>
                    (case #1 attrs of
                         (*L'.ERecord [("Signal", (L'.ESignalReturn e, _), _)] => (e, fm)
                       | L'.ERecord [("Signal", (L'.EApp ((L'.EAbs (_, _, _, (L'.ESignalReturn (L'.ERel 0, _), _)), _),
                                                          e), _), _)] => (e, fm) *)

                         L'.ERecord [("Signal", e, _)] =>
                         ((L'.EStrcat
                               ((L'.EPrim (Prim.String "<script type=\"text/javascript\">"), loc),
                                (L'.EStrcat ((L'.EJavaScript (L'.Script,
                                                              (L'.ELet ("signal", (L'.TSignal
                                                                                       (L'.TFfi ("Basis", "string"), loc),
                                                                                   loc),
                                                                        e,
                                                                        (L'.EWrite (L'.ERel 0, loc), loc)), loc)), loc),
                                             (L'.EPrim (Prim.String "</script>"), loc)), loc)), loc),
                          fm)
                       | _ => raise Fail "Monoize: Bad dyn attributes")
                    
                  | "submit" => normal ("input type=\"submit\"", NONE, NONE)

                  | "textbox" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         let
                             val (ts, fm) = tagStart "input"
                         in
                             ((L'.EStrcat (ts,
                                           (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\"/>")),
                                            loc)), loc), fm)
                         end
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to textarea tag"))
                  | "password" => input "password"
                  | "textarea" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         let
                             val (ts, fm) = tagStart "textarea"
                             val (xml, fm) = monoExp (env, st, fm) xml
                         in
                             ((L'.EStrcat ((L'.EStrcat (ts,
                                                        (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\">")), loc)), loc),
                                           (L'.EStrcat (xml,
                                                        (L'.EPrim (Prim.String "</textarea>"),
                                                         loc)), loc)),
                               loc), fm)
                         end
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to ltextarea tag"))

                  | "checkbox" => input "checkbox"

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
                                 SOME (L'.EPrim (Prim.String (" type=\"radio\" name=\"" ^ name ^ "\"")), loc),
                                 NONE))

                  | "select" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         let
                             val (ts, fm) = tagStart "select"
                             val (xml, fm) = monoExp (env, st, fm) xml
                         in
                             ((L'.EStrcat ((L'.EStrcat (ts,
                                                        (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\">")), loc)), loc),
                                           (L'.EStrcat (xml,
                                                        (L'.EPrim (Prim.String "</select>"),
                                                         loc)), loc)),
                               loc),
                              fm)
                         end
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to lselect tag"))

                  | "option" => normal ("option", NONE, NONE)

                  | "tabl" => normal ("table", NONE, NONE)
                  | _ => normal (tag, NONE, NONE)
            end

          | L.EApp ((L.ECApp (
                     (L.ECApp ((L.EFfi ("Basis", "form"), _), _), _),
                     _), _),
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

                val (action, actionT) = case findSubmit xml of
                    NotFound => raise Fail "No submit found"
                  | Error => raise Fail "Not ready for multi-submit lforms yet"
                  | Found et => et

                val actionT = monoType env actionT
                val (action, fm) = monoExp (env, st, fm) action
                val (action, fm) = urlifyExp env fm (action, actionT)
                val (xml, fm) = monoExp (env, st, fm) xml
            in
                ((L'.EStrcat ((L'.EStrcat ((L'.EPrim (Prim.String "<form action=\""), loc),
                                           (L'.EStrcat (action,
                                                        (L'.EPrim (Prim.String "\">"), loc)), loc)), loc),
                              (L'.EStrcat (xml,
                                           (L'.EPrim (Prim.String "</form>"), loc)), loc)), loc),
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
          | L.ECApp _ => poly ()
          | L.ECAbs _ => poly ()

          | L.EFfi mx => ((L'.EFfi mx, loc), fm)
          | L.EFfiApp (m, x, es) =>
            let
                val (es, fm) = ListUtil.foldlMap (fn (e, fm) => monoExp (env, st, fm) e) fm es
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
          | L.EFold _ => poly ()

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
            in
                ((L'.EClosure (n, es), loc), fm)
            end

          | L.ELet (x, t, e1, e2) =>
            let
                val t' = monoType env t
                val (e1, fm) = monoExp (env, st, fm) e1
                val (e2, fm) = monoExp (Env.pushERel env x t, st, fm) e2
            in
                ((L'.ELet (x, t', e1, e2), loc), fm)
            end
    end

fun monoDecl (env, fm) (all as (d, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported declaration";
             Print.eprefaces' [("Declaration", CorePrint.p_decl env all)];
             NONE)
    in
        case d of
            L.DCon _ => NONE
          | L.DDatatype (x, n, [], xncs) =>
            let
                val env' = Env.declBinds env all
                val d = (L'.DDatatype (x, n, map (fn (x, n, to) => (x, n, Option.map (monoType env') to)) xncs), loc)
            in
                SOME (env', fm, [d])
            end
          | L.DDatatype _ => poly ()
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
          | L.DExport (ek, n) =>
            let
                val (_, t, _, s) = Env.lookupENamed env n

                fun unwind (t, _) =
                    case t of
                        L.TFun (dom, ran) => dom :: unwind ran
                      | L.CApp ((L.CFfi ("Basis", "transaction"), _), t) =>
                        (L.TRecord (L.CRecord ((L.KType, loc), []), loc), loc) :: unwind t
                      | _ => []

                val ts = map (monoType env) (unwind t)
            in
                SOME (env, fm, [(L'.DExport (ek, s, n, ts), loc)])
            end
          | L.DTable (x, n, (L.CRecord (_, xts), _), s) =>
            let
                val t = (L.CFfi ("Basis", "string"), loc)
                val t' = (L'.TFfi ("Basis", "string"), loc)
                val s = "uw_" ^ s
                val e = (L'.EPrim (Prim.String s), loc)

                val xts = map (fn (x, t) => (monoName env x, monoType env t)) xts
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DTable (s, xts), loc),
                       (L'.DVal (x, n, t', e, s), loc)])
            end
          | L.DTable _ => poly ()
          | L.DSequence (x, n, s) =>
            let
                val t = (L.CFfi ("Basis", "string"), loc)
                val t' = (L'.TFfi ("Basis", "string"), loc)
                val s = "uw_" ^ s
                val e = (L'.EPrim (Prim.String s), loc)
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DSequence s, loc),
                       (L'.DVal (x, n, t', e, s), loc)])
            end
          | L.DDatabase s => SOME (env, fm, [(L'.DDatabase s, loc)])
          | L.DCookie (x, n, t, s) =>
            let
                val t = (L.CFfi ("Basis", "string"), loc)
                val t' = (L'.TFfi ("Basis", "string"), loc)
                val e = (L'.EPrim (Prim.String s), loc)
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      [(L'.DVal (x, n, t', e, s), loc)])
            end
    end

fun monoize env ds =
    let
        val p = !urlPrefix
        val () =
            if p = "" then
                urlPrefix := "/"
            else if String.sub (p, size p - 1) <> #"/" then
                urlPrefix := p ^ "/"
            else
                ()

        val (_, _, ds) = List.foldl (fn (d, (env, fm, ds)) =>
                                     case monoDecl (env, fm) d of
                                         NONE => (env, fm, ds)
                                       | SOME (env, fm, ds') =>
                                         (env,
                                          Fm.enter fm,
                                          ds' @ Fm.decls fm @ ds))
                                    (env, Fm.empty (CoreUtil.File.maxName ds + 1), []) ds
    in
        rev ds
    end

end
