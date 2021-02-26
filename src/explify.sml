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

structure Explify :> EXPLIFY = struct

structure EM = ErrorMsg
structure L = Elab
structure L' = Expl

fun explifyKind (k, loc) =
    case k of
        L.KType => (L'.KType, loc)
      | L.KArrow (k1, k2) => (L'.KArrow (explifyKind k1, explifyKind k2), loc)
      | L.KName => (L'.KName, loc)
      | L.KRecord k => (L'.KRecord (explifyKind k), loc)

      | L.KUnit => (L'.KUnit, loc)
      | L.KTuple ks => (L'.KTuple (map explifyKind ks), loc)

      | L.KError => raise Fail ("explifyKind: KError at " ^ EM.spanToString loc)
      | L.KUnif (_, _, ref (L.KKnown k)) => explifyKind k
      | L.KUnif _ => raise Fail ("explifyKind: KUnif at " ^ EM.spanToString loc)
      | L.KTupleUnif (loc, _, ref (L.KKnown k)) => explifyKind k
      | L.KTupleUnif _ => raise Fail ("explifyKind: KTupleUnif at " ^ EM.spanToString loc)

      | L.KRel n => (L'.KRel n, loc)
      | L.KFun (x, k) => (L'.KFun (x, explifyKind k), loc)

fun explifyCon (c, loc) =
    case c of
        L.TFun (t1, t2) => (L'.TFun (explifyCon t1, explifyCon t2), loc)
      | L.TCFun (_, x, k, t) => (L'.TCFun (x, explifyKind k, explifyCon t), loc)
      | L.TDisjoint (_, _, t) => explifyCon t
      | L.TRecord c => (L'.TRecord (explifyCon c), loc)

      | L.CRel n => (L'.CRel n, loc)
      | L.CNamed n => (L'.CNamed n, loc)
      | L.CModProj (m, ms, x) => (L'.CModProj (m, ms, x), loc)

      | L.CApp (c1, c2) => (L'.CApp (explifyCon c1, explifyCon c2), loc)
      | L.CAbs (x, k, c) => (L'.CAbs (x, explifyKind k, explifyCon c), loc)

      | L.CName s => (L'.CName s, loc)

      | L.CRecord (k, xcs) => (L'.CRecord (explifyKind k, map (fn (c1, c2) => (explifyCon c1, explifyCon c2)) xcs), loc)
      | L.CConcat (c1, c2) => (L'.CConcat (explifyCon c1, explifyCon c2), loc)
      | L.CMap (dom, ran) => (L'.CMap (explifyKind dom, explifyKind ran), loc)

      | L.CUnit => (L'.CUnit, loc)

      | L.CTuple cs => (L'.CTuple (map explifyCon cs), loc)
      | L.CProj (c, n) => (L'.CProj (explifyCon c, n), loc)

      | L.CError => raise Fail ("explifyCon: CError at " ^ EM.spanToString loc)
      | L.CUnif (nl, _, _, _, ref (L.Known c)) => explifyCon (ElabEnv.mliftConInCon nl c)
      | L.CUnif _ => raise Fail ("explifyCon: CUnif at " ^ EM.spanToString loc)

      | L.CKAbs (x, c) => (L'.CKAbs (x, explifyCon c), loc)
      | L.CKApp (c, k) => (L'.CKApp (explifyCon c, explifyKind k), loc)
      | L.TKFun (x, c) => (L'.TKFun (x, explifyCon c), loc)

fun explifyPatCon pc =
    case pc of
        L.PConVar n => L'.PConVar n
      | L.PConProj x => L'.PConProj x

fun explifyPat (p, loc) =
    case p of
        L.PVar (x, t) => (L'.PVar (x, explifyCon t), loc)
      | L.PPrim p => (L'.PPrim p, loc)
      | L.PCon (dk, pc, cs, po) => (L'.PCon (dk, explifyPatCon pc, map explifyCon cs, Option.map explifyPat po), loc)
      | L.PRecord xps => (L'.PRecord (map (fn (x, p, t) => (x, explifyPat p, explifyCon t)) xps), loc)

fun explifyExp (e, loc) =
    case e of
        L.EPrim p => (L'.EPrim p, loc)
      | L.ERel n => (L'.ERel n, loc)
      | L.ENamed n => (L'.ENamed n, loc)
      | L.EModProj (m, ms, x) => (L'.EModProj (m, ms, x), loc)
      | L.EApp (e1, e2) => (L'.EApp (explifyExp e1, explifyExp e2), loc)
      | L.EAbs (x, dom, ran, e1) => (L'.EAbs (x, explifyCon dom, explifyCon ran, explifyExp e1), loc)
      | L.ECApp (e1, c) => (L'.ECApp (explifyExp e1, explifyCon c), loc)
      | L.ECAbs (_, x, k, e1) => (L'.ECAbs (x, explifyKind k, explifyExp e1), loc)

      | L.ERecord xes => (L'.ERecord (map (fn (c, e, t) => (explifyCon c, explifyExp e, explifyCon t)) xes), loc)
      | L.EField (e1, c, {field, rest}) => (L'.EField (explifyExp e1, explifyCon c,
                                                       {field = explifyCon field, rest = explifyCon rest}), loc)
      | L.EConcat (e1, c1, e2, c2) => (L'.EConcat (explifyExp e1, explifyCon c1, explifyExp e2, explifyCon c2),
                                       loc)
      | L.ECut (e1, c, {field, rest}) => (L'.ECut (explifyExp e1, explifyCon c,
                                                     {field = explifyCon field, rest = explifyCon rest}), loc)
      | L.ECutMulti (e1, c, {rest}) => (L'.ECutMulti (explifyExp e1, explifyCon c,
                                                      {rest = explifyCon rest}), loc)
      | L.ECase (e, pes, {disc, result}) =>
        (L'.ECase (explifyExp e,
                   map (fn (p, e) => (explifyPat p, explifyExp e)) pes,
                   {disc = explifyCon disc, result = explifyCon result}), loc)

      | L.EError => raise Fail ("explifyExp: EError at " ^ EM.spanToString loc)
      | L.EUnif (ref (SOME e)) => explifyExp e
      | L.EUnif _ => raise Fail ("explifyExp: Undetermined EUnif at " ^ EM.spanToString loc)

      | L.ELet (des, e, t) =>
        foldr (fn ((de, loc), e) =>
                  case de of
                      L.EDValRec _ => raise Fail "explifyExp: Local 'val rec' remains"
                    | L.EDVal ((L.PVar (x, _), _), t', e') => (L'.ELet (x, explifyCon t', explifyExp e', e), loc)
                    | L.EDVal (p, t', e') => (L'.ECase (explifyExp e',
                                                        [(explifyPat p, e)],
                                                        {disc = explifyCon t', result = explifyCon t}), loc))
        (explifyExp e) des

      | L.EKAbs (x, e) => (L'.EKAbs (x, explifyExp e), loc)
      | L.EKApp (e, k) => (L'.EKApp (explifyExp e, explifyKind k), loc)

fun explifySgi (sgi, loc) =
    case sgi of
        L.SgiConAbs (x, n, k) => SOME (L'.SgiConAbs (x, n, explifyKind k), loc)
      | L.SgiCon (x, n, k, c) => SOME (L'.SgiCon (x, n, explifyKind k, explifyCon c), loc)
      | L.SgiDatatype dts => SOME (L'.SgiDatatype (map (fn (x, n, xs, xncs) =>
                                                           (x, n, xs,
                                                            map (fn (x, n, co) =>
                                                                    (x, n, Option.map explifyCon co)) xncs)) dts), loc)
      | L.SgiDatatypeImp (x, n, m1, ms, s, xs, xncs) =>
        SOME (L'.SgiDatatypeImp (x, n, m1, ms, s, xs, map (fn (x, n, co) =>
                                                              (x, n, Option.map explifyCon co)) xncs), loc)
      | L.SgiVal (x, n, c) => SOME (L'.SgiVal (x, n, explifyCon c), loc)
      | L.SgiStr (_, x, n, sgn) => SOME (L'.SgiStr (x, n, explifySgn sgn), loc)
      | L.SgiSgn (x, n, sgn) => SOME (L'.SgiSgn (x, n, explifySgn sgn), loc)
      | L.SgiConstraint _ => NONE
      | L.SgiClassAbs (x, n, k) => SOME (L'.SgiConAbs (x, n, (L'.KArrow (explifyKind k, (L'.KType, loc)), loc)), loc)
      | L.SgiClass (x, n, k, c) => SOME (L'.SgiCon (x, n, (L'.KArrow (explifyKind k, (L'.KType, loc)), loc),
                                                    explifyCon c), loc)

and explifySgn (sgn, loc) =
    case sgn of
        L.SgnConst sgis => (L'.SgnConst (List.mapPartial explifySgi sgis), loc)
      | L.SgnVar n => (L'.SgnVar n, loc)
      | L.SgnFun (m, n, dom, ran) => (L'.SgnFun (m, n, explifySgn dom, explifySgn ran), loc)
      | L.SgnWhere (sgn, ms, x, c) => (L'.SgnWhere (explifySgn sgn, ms, x, explifyCon c), loc)
      | L.SgnProj x => (L'.SgnProj x, loc)
      | L.SgnError => raise Fail ("explifySgn: SgnError at " ^ EM.spanToString loc)

fun explifyDecl (d, loc : EM.span) =
    case d of
        L.DCon (x, n, k, c) => SOME (L'.DCon (x, n, explifyKind k, explifyCon c), loc)
      | L.DDatatype dts => SOME (L'.DDatatype (map (fn (x, n, xs, xncs) =>
                                                       (x, n, xs,
                                                        map (fn (x, n, co) =>
                                                                (x, n, Option.map explifyCon co)) xncs)) dts), loc)
      | L.DDatatypeImp (x, n, m1, ms, s, xs, xncs) =>
        SOME (L'.DDatatypeImp (x, n, m1, ms, s, xs,
                               map (fn (x, n, co) =>
                                       (x, n, Option.map explifyCon co)) xncs), loc)
      | L.DVal (x, n, t, e) => SOME (L'.DVal (x, n, explifyCon t, explifyExp e), loc)
      | L.DValRec vis => SOME (L'.DValRec (map (fn (x, n, t, e) => (x, n, explifyCon t, explifyExp e)) vis), loc)

      | L.DSgn (x, n, sgn) => SOME (L'.DSgn (x, n, explifySgn sgn), loc)
      | L.DStr (x, n, sgn, str) => SOME (L'.DStr (x, n, explifySgn sgn, explifyStr str), loc)
      | L.DFfiStr (x, n, sgn) => SOME (L'.DFfiStr (x, n, explifySgn sgn), loc)
      | L.DConstraint (c1, c2) => NONE
      | L.DExport (en, sgn, str) => SOME (L'.DExport (en, explifySgn sgn, explifyStr str), loc)
      | L.DTable (nt, x, n, c, pe, pc, ce, cc) =>
        SOME (L'.DTable (nt, x, n, explifyCon c,
                         explifyExp pe, explifyCon pc,
                         explifyExp ce, explifyCon cc), loc)
      | L.DView (nt, x, n, e, c) =>
        SOME (L'.DView (nt, x, n, explifyExp e, explifyCon c), loc)
      | L.DIndex (e1, e2) =>
        SOME (L'.DIndex (explifyExp e1, explifyExp e2), loc)
      | L.DSequence (nt, x, n) => SOME (L'.DSequence (nt, x, n), loc)
      | L.DDatabase s => SOME (L'.DDatabase s, loc)
      | L.DCookie (nt, x, n, c) => SOME (L'.DCookie (nt, x, n, explifyCon c), loc)
      | L.DStyle (nt, x, n) => SOME (L'.DStyle (nt, x, n), loc)
      | L.DTask (e1, e2) => SOME (L'.DTask (explifyExp e1, explifyExp e2), loc)
      | L.DPolicy e1 => SOME (L'.DPolicy (explifyExp e1), loc)
      | L.DOnError v => SOME (L'.DOnError v, loc)
      | L.DFfi (x, n, modes, t) => SOME (L'.DFfi (x, n, modes, explifyCon t), loc)

and explifyStr (str, loc) =
    case str of
        L.StrConst ds => (L'.StrConst (List.mapPartial explifyDecl ds), loc)
      | L.StrVar n => (L'.StrVar n, loc)
      | L.StrProj (str, s) => (L'.StrProj (explifyStr str, s), loc)
      | L.StrFun (m, n, dom, ran, str) => (L'.StrFun (m, n, explifySgn dom, explifySgn ran, explifyStr str), loc)
      | L.StrApp (str1, str2) => (L'.StrApp (explifyStr str1, explifyStr str2), loc)
      | L.StrError => raise Fail ("explifyStr: StrError at " ^ EM.spanToString loc)

val explify = List.mapPartial explifyDecl

end
