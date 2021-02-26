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

structure ElabUtilPos :> ELAB_UTIL_POS = struct

open Elab

structure S = Search

structure Kind = struct

fun mapfoldB {kind, bind} =
    let
        fun mfk ctx k acc =
            S.bindPWithPos (mfk' ctx k acc, kind ctx)

        and mfk' ctx (kAll as (k, loc)) =
            case k of
                KType => S.return2 kAll

              | KArrow (k1, k2) =>
                S.bind2 (mfk ctx k1,
                      fn k1' =>
                         S.map2 (mfk ctx k2,
                              fn k2' =>
                                 (KArrow (k1', k2'), loc)))

              | KName => S.return2 kAll

              | KRecord k =>
                S.map2 (mfk ctx k,
                        fn k' =>
                           (KRecord k', loc))

              | KUnit => S.return2 kAll

              | KTuple ks =>
                S.map2 (ListUtil.mapfold (mfk ctx) ks,
                        fn ks' =>
                           (KTuple ks', loc))

              | KError => S.return2 kAll

              | KUnif (_, _, ref (KKnown k)) => mfk' ctx k
              | KUnif _ => S.return2 kAll

              | KTupleUnif (_, _, ref (KKnown k)) => mfk' ctx k
              | KTupleUnif (loc, nks, r) =>
                S.map2 (ListUtil.mapfold (fn (n, k) =>
                                             S.map2 (mfk ctx k,
                                                  fn k' =>
                                                     (n, k'))) nks,
                     fn nks' =>
                        (KTupleUnif (loc, nks', r), loc))


              | KRel _ => S.return2 kAll
              | KFun (x, k) =>
                S.map2 (mfk (bind (ctx, x)) k,
                        fn k' =>
                           (KFun (x, k'), loc))
    in
        mfk
    end

end

val mliftConInCon = ref (fn n : int => fn c : con => (raise Fail "You didn't set ElabUtil.mliftConInCon!") : con)

structure Con = struct

datatype binder =
         RelK of string
       | RelC of string * Elab.kind
       | NamedC of string * int * Elab.kind * Elab.con option

fun mapfoldB {kind = fk, con = fc, bind} =
    let
        val mfk = Kind.mapfoldB {kind = fk, bind = fn (ctx, s) => bind (ctx, RelK s)}

        fun mfc ctx c acc =
            S.bindPWithPos (mfc' ctx c acc, fc ctx)

        and mfc' ctx (cAll as (c, loc)) =
            case c of
                TFun (c1, c2) =>
                S.bind2 (mfc ctx c1,
                      fn c1' =>
                         S.map2 (mfc ctx c2,
                              fn c2' =>
                                 (TFun (c1', c2'), loc)))
              | TCFun (e, x, k, c) =>
                S.bind2 (mfk ctx k,
                      fn k' =>
                         S.map2 (mfc (bind (ctx, RelC (x, k))) c,
                              fn c' =>
                                 (TCFun (e, x, k', c'), loc)))
              | TDisjoint (c1, c2, c3) =>
                S.bind2 (mfc ctx c1,
                      fn c1' =>
                         S.bind2 (mfc ctx c2,
                              fn c2' =>
                                 S.map2 (mfc ctx c3,
                                         fn c3' =>
                                            (TDisjoint (c1', c2', c3'), loc))))
              | TRecord c =>
                S.map2 (mfc ctx c,
                        fn c' =>
                           (TRecord c', loc))

              | CRel _ => S.return2 cAll
              | CNamed _ => S.return2 cAll
              | CModProj _ => S.return2 cAll
              | CApp (c1, c2) =>
                S.bind2 (mfc ctx c1,
                      fn c1' =>
                         S.map2 (mfc ctx c2,
                              fn c2' =>
                                 (CApp (c1', c2'), loc)))
              | CAbs (x, k, c) =>
                S.bind2 (mfk ctx k,
                      fn k' =>
                         S.map2 (mfc (bind (ctx, RelC (x, k))) c,
                              fn c' =>
                                 (CAbs (x, k', c'), loc)))

              | CName _ => S.return2 cAll

              | CRecord (k, xcs) =>
                S.bind2 (mfk ctx k,
                      fn k' =>
                         S.map2 (ListUtil.mapfold (fn (x, c) =>
                                                      S.bind2 (mfc ctx x,
                                                            fn x' =>
                                                               S.map2 (mfc ctx c,
                                                                    fn c' =>
                                                                       (x', c'))))
                                 xcs,
                              fn xcs' =>
                                 (CRecord (k', xcs'), loc)))
              | CConcat (c1, c2) =>
                S.bind2 (mfc ctx c1,
                      fn c1' =>
                         S.map2 (mfc ctx c2,
                              fn c2' =>
                                 (CConcat (c1', c2'), loc)))
              | CMap (k1, k2) =>
                S.bind2 (mfk ctx k1,
                         fn k1' =>
                            S.map2 (mfk ctx k2,
                                    fn k2' =>
                                       (CMap (k1', k2'), loc)))

              | CUnit => S.return2 cAll

              | CTuple cs =>
                S.map2 (ListUtil.mapfold (mfc ctx) cs,
                        fn cs' =>
                           (CTuple cs', loc))

              | CProj (c, n) =>
                S.map2 (mfc ctx c,
                        fn c' =>
                           (CProj (c', n), loc))

              | CError => S.return2 cAll
              | CUnif (nl, _, _, _, ref (Known c)) => mfc' ctx (!mliftConInCon nl c)
              | CUnif _ => S.return2 cAll
                        
              | CKAbs (x, c) =>
                S.map2 (mfc (bind (ctx, RelK x)) c,
                        fn c' =>
                           (CKAbs (x, c'), loc))
              | CKApp (c, k) =>
                S.bind2 (mfc ctx c,
                      fn c' =>
                         S.map2 (mfk ctx k,
                                 fn k' =>
                                    (CKApp (c', k'), loc)))
              | TKFun (x, c) =>
                S.map2 (mfc (bind (ctx, RelK x)) c,
                        fn c' =>
                           (TKFun (x, c'), loc))
    in
        mfc
    end

end

structure Exp = struct

datatype binder =
         RelK of string
       | RelC of string * Elab.kind
       | NamedC of string * int * Elab.kind * Elab.con option
       | RelE of string * Elab.con
       | NamedE of string * Elab.con

fun mapfoldB {kind = fk, con = fc, exp = fe, bind} =
    let
        val mfk = Kind.mapfoldB {kind = fk, bind = fn (ctx, x) => bind (ctx, RelK x)}

        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Con.RelK x => RelK x
                           | Con.RelC x => RelC x
                           | Con.NamedC x => NamedC x
            in
                bind (ctx, b')
            end
        val mfc = Con.mapfoldB {kind = fk, con = fc, bind = bind'}

        fun doVars ((p, _), ctx) =
            case p of
                PVar xt => bind (ctx, RelE xt)
              | PPrim _ => ctx
              | PCon (_, _, _, NONE) => ctx
              | PCon (_, _, _, SOME p) => doVars (p, ctx)
              | PRecord xpcs =>
                foldl (fn ((_, p, _), ctx) => doVars (p, ctx))
                      ctx xpcs

        fun mfe ctx e acc =
            S.bindPWithPos (mfe' ctx e acc, fe ctx)

        and mfe' ctx (eAll as (e, loc)) =
            case e of
                EPrim _ => S.return2 eAll
              | ERel _ => S.return2 eAll
              | ENamed _ => S.return2 eAll
              | EModProj _ => S.return2 eAll
              | EApp (e1, e2) =>
                S.bind2 (mfe ctx e1,
                      fn e1' =>
                         S.map2 (mfe ctx e2,
                              fn e2' =>
                                 (EApp (e1', e2'), loc)))
              | EAbs (x, dom, ran, e) =>
                S.bind2 (mfc ctx dom,
                      fn dom' =>
                         S.bind2 (mfc ctx ran,
                                  fn ran' =>
                                     S.map2 (mfe (bind (ctx, RelE (x, dom'))) e,
                                          fn e' =>
                                             (EAbs (x, dom', ran', e'), loc))))
                         
              | ECApp (e, c) =>
                S.bind2 (mfe ctx e,
                      fn e' =>
                         S.map2 (mfc ctx c,
                              fn c' =>
                                 (ECApp (e', c'), loc)))
              | ECAbs (expl, x, k, e) =>
                S.bind2 (mfk ctx k,
                      fn k' =>
                         S.map2 (mfe (bind (ctx, RelC (x, k))) e,
                              fn e' =>
                                 (ECAbs (expl, x, k', e'), loc)))

              | ERecord xes =>
                S.map2 (ListUtil.mapfold (fn (x, e, t) =>
                                             S.bind2 (mfc ctx x,
                                                   fn x' =>
                                                      S.bind2 (mfe ctx e,
                                                           fn e' =>
                                                              S.map2 (mfc ctx t,
                                                                   fn t' =>
                                                                      (x', e', t')))))
                                         xes,
                     fn xes' =>
                        (ERecord xes', loc))
              | EField (e, c, {field, rest}) =>
                S.bind2 (mfe ctx e,
                      fn e' =>
                         S.bind2 (mfc ctx c,
                              fn c' =>
                                 S.bind2 (mfc ctx field,
                                          fn field' =>
                                             S.map2 (mfc ctx rest,
                                                  fn rest' =>
                                                     (EField (e', c', {field = field', rest = rest'}), loc)))))
              | EConcat (e1, c1, e2, c2) =>
                S.bind2 (mfe ctx e1,
                      fn e1' =>
                         S.bind2 (mfc ctx c1,
                              fn c1' =>
                                 S.bind2 (mfe ctx e2,
                                       fn e2' =>
                                          S.map2 (mfc ctx c2,
                                               fn c2' =>
                                                  (EConcat (e1', c1', e2', c2'),
                                                   loc)))))
              | ECut (e, c, {field, rest}) =>
                S.bind2 (mfe ctx e,
                      fn e' =>
                         S.bind2 (mfc ctx c,
                              fn c' =>
                                 S.bind2 (mfc ctx field,
                                          fn field' =>
                                             S.map2 (mfc ctx rest,
                                                  fn rest' =>
                                                     (ECut (e', c', {field = field', rest = rest'}), loc)))))

              | ECutMulti (e, c, {rest}) =>
                S.bind2 (mfe ctx e,
                      fn e' =>
                         S.bind2 (mfc ctx c,
                              fn c' =>
                                 S.map2 (mfc ctx rest,
                                      fn rest' =>
                                         (ECutMulti (e', c', {rest = rest'}), loc))))

              | ECase (e, pes, {disc, result}) =>
                S.bind2 (mfe ctx e,
                         fn e' =>
                            S.bind2 (ListUtil.mapfold (fn (p, e) =>
                                                          let
                                                              fun pb ((p, _), ctx) =
                                                                  case p of
                                                                      PVar (x, t) => bind (ctx, RelE (x, t))
                                                                    | PPrim _ => ctx
                                                                    | PCon (_, _, _, NONE) => ctx
                                                                    | PCon (_, _, _, SOME p) => pb (p, ctx)
                                                                    | PRecord xps => foldl (fn ((_, p, _), ctx) =>
                                                                                               pb (p, ctx)) ctx xps
                                                          in
                                                              S.bind2 (mfp ctx p,
                                                                       fn p' =>
                                                                          S.map2 (mfe (pb (p', ctx)) e,
                                                                               fn e' => (p', e')))
                                                          end) pes,
                                    fn pes' =>
                                       S.bind2 (mfc ctx disc,
                                             fn disc' =>
                                                S.map2 (mfc ctx result,
                                                     fn result' =>
                                                        (ECase (e', pes', {disc = disc', result = result'}), loc)))))

              | EError => S.return2 eAll
              | EUnif (ref (SOME e)) => mfe ctx e
              | EUnif _ => S.return2 eAll

              | ELet (des, e, t) =>
                let
                    val (des, ctx') = foldl (fn (ed, (des, ctx)) =>
                                                let
                                                    val ctx' =
                                                        case #1 ed of
                                                            EDVal (p, _, _) => doVars (p, ctx)
                                                          | EDValRec vis =>
                                                            foldl (fn ((x, t, _), ctx) => bind (ctx, RelE (x, t)))
                                                                  ctx vis
                                                in
                                                    (S.bind2 (des,
                                                           fn des' =>
                                                              S.map2 (mfed ctx ed,
                                                                   fn ed' => ed' :: des')),
                                                     ctx')
                                                end)
                                            (S.return2 [], ctx) des
                in
                    S.bind2 (des,
                         fn des' =>
                            S.bind2 (mfe ctx' e,
                                    fn e' =>
                                       S.map2 (mfc ctx t,
                                               fn t' =>
                                                  (ELet (rev des', e', t'), loc))))
                end

              | EKAbs (x, e) =>
                S.map2 (mfe (bind (ctx, RelK x)) e,
                        fn e' =>
                           (EKAbs (x, e'), loc))
              | EKApp (e, k) =>
                S.bind2 (mfe ctx e,
                        fn e' =>
                           S.map2 (mfk ctx k,
                                   fn k' =>
                                      (EKApp (e', k'), loc)))

        and mfp ctx (pAll as (p, loc)) =
            case p of
                PVar (x, t) =>
                S.map2 (mfc ctx t,
                        fn t' =>
                           (PVar (x, t'), loc))
              | PPrim _ => S.return2 pAll
              | PCon (dk, pc, args, po) =>
                S.bind2 (ListUtil.mapfold (mfc ctx) args,
                      fn args' =>
                         S.map2 ((case po of
                                      NONE => S.return2 NONE
                                    | SOME p => S.map2 (mfp ctx p, SOME)),
                              fn po' =>
                                 (PCon (dk, pc, args', po'), loc)))
              | PRecord xps =>
                S.map2 (ListUtil.mapfold (fn (x, p, c) =>
                                              S.bind2 (mfp ctx p,
                                                       fn p' =>
                                                          S.map2 (mfc ctx c,
                                                                  fn c' =>
                                                                     (x, p', c')))) xps,
                         fn xps' =>
                            (PRecord xps', loc))

        and mfed ctx (dAll as (d, loc)) =
            case d of
                EDVal (p, t, e) =>
                S.bind2 (mfc ctx t,
                         fn t' =>
                            S.map2 (mfe ctx e,
                                 fn e' =>
                                    (EDVal (p, t', e'), loc)))
              | EDValRec vis =>
                let
                    val ctx = foldl (fn ((x, t, _), ctx) => bind (ctx, RelE (x, t))) ctx vis
                in
                    S.map2 (ListUtil.mapfold (mfvi ctx) vis,
                         fn vis' =>
                            (EDValRec vis', loc))
                end

        and mfvi ctx (x, c, e) =
            S.bind2 (mfc ctx c,
                  fn c' =>
                     S.map2 (mfe ctx e,
                          fn e' =>
                             (x, c', e')))
    in
        mfe
    end

end

structure Sgn = struct

datatype binder =
         RelK of string
       | RelC of string * Elab.kind
       | NamedC of string * int * Elab.kind * Elab.con option
       | Str of string * int * Elab.sgn
       | Sgn of string * int * Elab.sgn

fun mapfoldB {kind, con, sgn_item, sgn, bind} =
    let
        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Con.RelK x => RelK x
                           | Con.RelC x => RelC x
                           | Con.NamedC x => NamedC x
            in
                bind (ctx, b')
            end
        val con = Con.mapfoldB {kind = kind, con = con, bind = bind'}

        val kind = Kind.mapfoldB {kind = kind, bind = fn (ctx, x) => bind (ctx, RelK x)}

        fun sgi ctx si acc =
            S.bindPWithPos (sgi' ctx si acc, sgn_item ctx)

        and sgi' ctx (siAll as (si, loc)) =
            case si of
                SgiConAbs (x, n, k) =>
                S.map2 (kind ctx k,
                     fn k' =>
                        (SgiConAbs (x, n, k'), loc))
              | SgiCon (x, n, k, c) =>
                S.bind2 (kind ctx k,
                     fn k' =>
                        S.map2 (con ctx c,
                             fn c' =>
                                (SgiCon (x, n, k', c'), loc)))
              | SgiDatatype dts =>
                S.map2 (ListUtil.mapfold (fn (x, n, xs, xncs) =>
                                             S.map2 (ListUtil.mapfold (fn (x, n, c) =>
                                                                          case c of
                                                                              NONE => S.return2 (x, n, c)
                                                                            | SOME c =>
                                                                              S.map2 (con ctx c,
                                                                                   fn c' => (x, n, SOME c'))) xncs,
                                                  fn xncs' => (x, n, xs, xncs'))) dts,
                        fn dts' =>
                           (SgiDatatype dts', loc))
              | SgiDatatypeImp (x, n, m1, ms, s, xs, xncs) =>
                S.map2 (ListUtil.mapfold (fn (x, n, c) =>
                                             case c of
                                                 NONE => S.return2 (x, n, c)
                                               | SOME c =>
                                                 S.map2 (con ctx c,
                                                      fn c' => (x, n, SOME c'))) xncs,
                        fn xncs' =>
                           (SgiDatatypeImp (x, n, m1, ms, s, xs, xncs'), loc))
              | SgiVal (x, n, c) =>
                S.map2 (con ctx c,
                     fn c' =>
                        (SgiVal (x, n, c'), loc))
              | SgiStr (im, x, n, s) =>
                S.map2 (sg ctx s,
                     fn s' =>
                        (SgiStr (im, x, n, s'), loc))
              | SgiSgn (x, n, s) =>
                S.map2 (sg ctx s,
                     fn s' =>
                        (SgiSgn (x, n, s'), loc))
              | SgiConstraint (c1, c2) =>
                S.bind2 (con ctx c1,
                         fn c1' =>
                            S.map2 (con ctx c2,
                                    fn c2' =>
                                       (SgiConstraint (c1', c2'), loc)))
              | SgiClassAbs (x, n, k) =>
                S.map2 (kind ctx k,
                        fn k' =>
                           (SgiClassAbs (x, n, k'), loc))
              | SgiClass (x, n, k, c) =>
                S.bind2 (kind ctx k,
                      fn k' => 
                         S.map2 (con ctx c,
                              fn c' =>
                                 (SgiClass (x, n, k', c'), loc)))

        and sg ctx s acc =
            S.bindPWithPos (sg' ctx s acc, sgn ctx)

        and sg' ctx (sAll as (s, loc)) =
            case s of
                SgnConst sgis =>
                S.map2 (ListUtil.mapfoldB (fn (ctx, si)  =>
                                              (case #1 si of
                                                   SgiConAbs (x, n, k) =>
                                                   bind (ctx, NamedC (x, n, k, NONE))
                                                 | SgiCon (x, n, k, c) =>
                                                   bind (ctx, NamedC (x, n, k, SOME c))
                                                 | SgiDatatype dts =>
                                                   foldl (fn ((x, n, ks, _), ctx) =>
                                                             let
                                                                 val k' = (KType, loc)
                                                                 val k = foldl (fn (_, k) => (KArrow (k', k), loc))
                                                                               k' ks
                                                             in
                                                                 bind (ctx, NamedC (x, n, k, NONE))
                                                             end) ctx dts
                                                 | SgiDatatypeImp (x, n, m1, ms, s, _, _) =>
                                                   bind (ctx, NamedC (x, n, (KType, loc),
                                                                      SOME (CModProj (m1, ms, s), loc)))
                                                 | SgiVal _ => ctx
                                                 | SgiStr (_, x, n, sgn) =>
                                                   bind (ctx, Str (x, n, sgn))
                                                 | SgiSgn (x, n, sgn) =>
                                                   bind (ctx, Sgn (x, n, sgn))
                                                 | SgiConstraint _ => ctx
                                                 | SgiClassAbs (x, n, k) =>
                                                   bind (ctx, NamedC (x, n, (KArrow (k, (KType, loc)), loc), NONE))
                                                 | SgiClass (x, n, k, c) =>
                                                   bind (ctx, NamedC (x, n, (KArrow (k, (KType, loc)), loc), SOME c)),
                                               sgi ctx si)) ctx sgis,
                     fn sgis' =>
                        (SgnConst sgis', loc))
                
              | SgnVar _ => S.return2 sAll
              | SgnFun (m, n, s1, s2) =>
                S.bind2 (sg ctx s1,
                         fn s1' =>
                            S.map2 (sg (bind (ctx, Str (m, n, s1'))) s2,
                                    fn s2' =>
                                       (SgnFun (m, n, s1', s2'), loc)))
              | SgnProj _ => S.return2 sAll
              | SgnWhere (sgn, ms, x, c) =>
                S.bind2 (sg ctx sgn,
                      fn sgn' =>
                         S.map2 (con ctx c,
                              fn c' =>
                                 (SgnWhere (sgn', ms, x, c'), loc)))
              | SgnError => S.return2 sAll
    in
        sg
    end

end

structure Decl = struct

datatype binder =
         RelK of string
       | RelC of string * Elab.kind
       | NamedC of string * int * Elab.kind * Elab.con option
       | RelE of string * Elab.con
       | NamedE of string * Elab.con
       | Str of string * int * Elab.sgn
       | Sgn of string * int * Elab.sgn

fun mapfoldB {kind = fk, con = fc, exp = fe, sgn_item = fsgi, sgn = fsg, str = fst, decl = fd, bind} =
    let
        val mfk = Kind.mapfoldB {kind = fk, bind = fn (ctx, x) => bind (ctx, RelK x)}

        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Con.RelK x => RelK x
                           | Con.RelC x => RelC x
                           | Con.NamedC x => NamedC x
            in
                bind (ctx, b')
            end
        val mfc = Con.mapfoldB {kind = fk, con = fc, bind = bind'}

        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Exp.RelK x => RelK x
                           | Exp.RelC x => RelC x
                           | Exp.NamedC x => NamedC x
                           | Exp.RelE x => RelE x
                           | Exp.NamedE x => NamedE x
            in
                bind (ctx, b')
            end
        val mfe = Exp.mapfoldB {kind = fk, con = fc, exp = fe, bind = bind'}

        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Sgn.RelK x => RelK x
                           | Sgn.RelC x => RelC x
                           | Sgn.NamedC x => NamedC x
                           | Sgn.Sgn x => Sgn x
                           | Sgn.Str x => Str x
            in
                bind (ctx, b')
            end
        val mfsg = Sgn.mapfoldB {kind = fk, con = fc, sgn_item = fsgi, sgn = fsg, bind = bind'}

        fun mfst ctx str acc =
            S.bindPWithPos (mfst' ctx str acc, fst ctx)

        and mfst' ctx (strAll as (str, loc)) =
            case str of
                StrConst ds => 
                S.map2 (ListUtil.mapfoldB (fn (ctx, d)  =>
                                              (case #1 d of
                                                   DCon (x, n, k, c) =>
                                                   bind (ctx, NamedC (x, n, k, SOME c))
                                                 | DDatatype dts =>
                                                   let
                                                       fun doOne ((x, n, xs, xncs), ctx) =
                                                           let
                                                               val ctx = bind (ctx, NamedC (x, n, (KType, loc), NONE))
                                                           in
                                                               foldl (fn ((x, _, co), ctx) =>
                                                                         let
                                                                             val t =
                                                                                 case co of
                                                                                     NONE => CNamed n
                                                                                   | SOME t => TFun (t, (CNamed n, loc))
                                                                                               
                                                                             val k = (KType, loc)
                                                                             val t = (t, loc)
                                                                             val t = foldr (fn (x, t) =>
                                                                                               (TCFun (Explicit,
                                                                                                       x,
                                                                                                       k,
                                                                                                       t), loc))
                                                                                           t xs
                                                                         in
                                                                             bind (ctx, NamedE (x, t))
                                                                         end)
                                                                     ctx xncs
                                                           end
                                                   in
                                                       foldl doOne ctx dts
                                                   end
                                                 | DDatatypeImp (x, n, m, ms, x', _, _) =>
                                                   bind (ctx, NamedC (x, n, (KType, loc),
                                                                      SOME (CModProj (m, ms, x'), loc)))
                                                 | DVal (x, _, c, _) =>
                                                   bind (ctx, NamedE (x, c))
                                                 | DValRec vis =>
                                                   foldl (fn ((x, _, c, _), ctx) => bind (ctx, NamedE (x, c))) ctx vis
                                                 | DSgn (x, n, sgn) =>
                                                   bind (ctx, Sgn (x, n, sgn))
                                                 | DStr (x, n, sgn, _) =>
                                                   bind (ctx, Str (x, n, sgn))
                                                 | DFfiStr (x, n, sgn) =>
                                                   bind (ctx, Str (x, n, sgn))
                                                 | DConstraint _ => ctx
                                                 | DExport _ => ctx
                                                 | DTable (tn, x, n, c, _, pc, _, cc) =>
                                                   let
                                                       val ct = (CModProj (n, [], "sql_table"), loc)
                                                       val ct = (CApp (ct, c), loc)
                                                       val ct = (CApp (ct, (CConcat (pc, cc), loc)), loc)
                                                   in
                                                       bind (ctx, NamedE (x, ct))
                                                   end
                                                 | DSequence (tn, x, n) =>
                                                   bind (ctx, NamedE (x, (CModProj (n, [], "sql_sequence"), loc)))
                                                 | DView (tn, x, n, _, c) =>
                                                   let
                                                       val ct = (CModProj (n, [], "sql_view"), loc)
                                                       val ct = (CApp (ct, c), loc)
                                                   in
                                                       bind (ctx, NamedE (x, ct))
                                                   end
                                                 | DIndex _ => ctx
                                                 | DDatabase _ => ctx
                                                 | DCookie (tn, x, n, c) =>
                                                   bind (ctx, NamedE (x, (CApp ((CModProj (n, [], "cookie"), loc),
                                                                                c), loc)))
                                                 | DStyle (tn, x, n) =>
                                                   bind (ctx, NamedE (x, (CModProj (n, [], "css_class"), loc)))
                                                 | DTask _ => ctx
                                                 | DPolicy _ => ctx
                                                 | DOnError _ => ctx
                                                 | DFfi (x, _, _, t) => bind (ctx, NamedE (x, t)),
                                               mfd ctx d)) ctx ds,
                     fn ds' => (StrConst ds', loc))
              | StrVar _ => S.return2 strAll
              | StrProj (str, x) =>
                S.map2 (mfst ctx str,
                        fn str' =>
                           (StrProj (str', x), loc))
              | StrFun (x, n, sgn1, sgn2, str) =>
                S.bind2 (mfsg ctx sgn1,
                         fn sgn1' =>
                            S.bind2 (mfsg ctx sgn2,
                                     fn sgn2' =>
                                        S.map2 (mfst ctx str,
                                                fn str' =>
                                                   (StrFun (x, n, sgn1', sgn2', str'), loc))))
              | StrApp (str1, str2) =>
                S.bind2 (mfst ctx str1,
                         fn str1' =>
                            S.map2 (mfst ctx str2,
                                    fn str2' =>
                                       (StrApp (str1', str2'), loc)))
              | StrError => S.return2 strAll

        and mfd ctx d acc =
            S.bindPWithPos (mfd' ctx d acc, fd ctx)

        and mfd' ctx (dAll as (d, loc)) =
            case d of
                DCon (x, n, k, c) =>
                S.bind2 (mfk ctx k,
                         fn k' =>
                            S.map2 (mfc ctx c,
                                    fn c' =>
                                       (DCon (x, n, k', c'), loc)))
              | DDatatype dts =>
                S.map2 (ListUtil.mapfold (fn (x, n, xs, xncs) =>
                                             S.map2 (ListUtil.mapfold (fn (x, n, c) =>
                                                                          case c of
                                                                              NONE => S.return2 (x, n, c)
                                                                            | SOME c =>
                                                                              S.map2 (mfc ctx c,
                                                                                   fn c' => (x, n, SOME c'))) xncs,
                                                     fn xncs' =>
                                                        (x, n, xs, xncs'))) dts,
                     fn dts' =>
                        (DDatatype dts', loc))
              | DDatatypeImp (x, n, m1, ms, s, xs, xncs) =>
                S.map2 (ListUtil.mapfold (fn (x, n, c) =>
                                             case c of
                                                 NONE => S.return2 (x, n, c)
                                               | SOME c =>
                                                 S.map2 (mfc ctx c,
                                                      fn c' => (x, n, SOME c'))) xncs,
                        fn xncs' =>
                           (DDatatypeImp (x, n, m1, ms, s, xs, xncs'), loc))
              | DVal vi =>
                S.map2 (mfvi ctx vi,
                     fn vi' =>
                        (DVal vi', loc))
              | DValRec vis =>
                S.map2 (ListUtil.mapfold (mfvi ctx) vis,
                     fn vis' =>
                        (DValRec vis', loc))
              | DSgn (x, n, sgn) =>
                S.map2 (mfsg ctx sgn,
                        fn sgn' =>
                           (DSgn (x, n, sgn'), loc))
              | DStr (x, n, sgn, str) =>
                S.bind2 (mfsg ctx sgn,
                         fn sgn' =>
                            S.map2 (mfst ctx str,
                                    fn str' =>
                                       (DStr (x, n, sgn', str'), loc)))
              | DFfiStr (x, n, sgn) =>
                S.map2 (mfsg ctx sgn,
                        fn sgn' =>
                           (DFfiStr (x, n, sgn'), loc))
              | DConstraint (c1, c2) =>
                S.bind2 (mfc ctx c1,
                         fn c1' =>
                            S.map2 (mfc ctx c2,
                                    fn c2' =>
                                       (DConstraint (c1', c2'), loc)))
              | DExport (en, sgn, str) =>
                S.bind2 (mfsg ctx sgn,
                         fn sgn' =>
                            S.map2 (mfst ctx str,
                                    fn str' =>
                                       (DExport (en, sgn', str'), loc)))

              | DTable (tn, x, n, c, pe, pc, ce, cc) =>
                S.bind2 (mfc ctx c,
                        fn c' =>
                           S.bind2 (mfe ctx pe,
                                   fn pe' =>
                                      S.bind2 (mfc ctx pc,
                                            fn pc' =>
                                               S.bind2 (mfe ctx ce,
                                                     fn ce' =>
                                                        S.map2 (mfc ctx cc,
                                                              fn cc' =>
                                                                 (DTable (tn, x, n, c', pe', pc', ce', cc'), loc))))))
              | DSequence _ => S.return2 dAll
              | DView (tn, x, n, e, c) =>
                S.bind2 (mfe ctx e,
                        fn e' =>
                           S.map2 (mfc ctx c,
                                   fn c' =>
                                      (DView (tn, x, n, e', c'), loc)))
              | DIndex (e1, e2) =>
                S.bind2 (mfe ctx e1,
                        fn e1' =>
                           S.map2 (mfe ctx e2,
                                   fn e2' =>
                                      (DIndex (e1', e2'), loc)))

              | DDatabase _ => S.return2 dAll

              | DCookie (tn, x, n, c) =>
                S.map2 (mfc ctx c,
                        fn c' =>
                           (DCookie (tn, x, n, c'), loc))
              | DStyle _ => S.return2 dAll
              | DTask (e1, e2) =>
                S.bind2 (mfe ctx e1,
                        fn e1' =>
                           S.map2 (mfe ctx e2,
                                fn e2' =>
                                   (DTask (e1', e2'), loc)))
              | DPolicy e1 =>
                S.map2 (mfe ctx e1,
                     fn e1' =>
                        (DPolicy e1', loc))
              | DOnError _ => S.return2 dAll
              | DFfi (x, n, modes, t) =>
                S.map2 (mfc ctx t,
                        fn t' =>
                           (DFfi (x, n, modes, t'), loc))

        and mfvi ctx (x, n, c, e) =
            S.bind2 (mfc ctx c,
                  fn c' =>
                     S.map2 (mfe ctx e,
                          fn e' =>
                             (x, n, c', e')))
    in
        mfd
    end

    fun fold {kind, con, exp, sgn_item, sgn, str, decl} (st : 'a) d : 'a =
        case mapfoldB {kind = fn () => fn k => fn st => S.Continue (#1 k, kind (k, st)),
                      con = fn () => fn c => fn st => S.Continue (#1 c, con (c, st)),
                      exp = fn () => fn e => fn st => S.Continue (#1 e, exp (e, st)),
                      sgn_item = fn () => fn sgi => fn st => S.Continue (#1 sgi, sgn_item (sgi, st)),
                      sgn = fn () => fn s => fn st => S.Continue (#1 s, sgn (s, st)),
                      str = fn () => fn str' => fn st => S.Continue (#1 str', str (str', st)),
                      decl = fn () => fn d => fn st => S.Continue (#1 d, decl (d, st)),
                      bind = fn ((), _) => ()
                      } () d st of
            S.Continue (_, st) => st
          | S.Return _ => raise Fail "ElabUtil.Decl.fold: Impossible"

    fun foldB {kind, con, exp, sgn_item, sgn, str, decl, bind} ctx (st : 'a) d : 'a =
        case mapfoldB {kind = fn ctx => fn k => fn st => S.Continue (#1 k, kind (ctx, k, st)),
                      con = fn ctx => fn c => fn st => S.Continue (#1 c, con (ctx, c, st)),
                      exp = fn ctx => fn e => fn st => S.Continue (#1 e, exp (ctx, e, st)),
                      sgn_item = fn ctx => fn sgi => fn st => S.Continue (#1 sgi, sgn_item (ctx, sgi, st)),
                      sgn = fn ctx => fn s => fn st => S.Continue (#1 s, sgn (ctx, s, st)),
                      str = fn ctx => fn str' => fn st => S.Continue (#1 str', str (ctx, str', st)),
                      decl = fn ctx => fn d => fn st => S.Continue (#1 d, decl (ctx, d, st)),
                      bind = bind
                      } ctx d st of
            S.Continue (_, st) => st
          | S.Return _ => raise Fail "ElabUtil.Decl.foldB: Impossible"
    end
end
