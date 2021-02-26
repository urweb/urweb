(* Copyright (c) 2008-2010, 2013, Adam Chlipala
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

structure CoreUtil :> CORE_UTIL = struct

open Core

structure S = Search

structure Kind = struct

open Order

fun compare ((k1, _), (k2, _)) =
    case (k1, k2) of
        (KType, KType) => EQUAL
      | (KType, _) => LESS
      | (_, KType) => GREATER

      | (KArrow (d1, r1), KArrow (d2, r2)) => join (compare (d1, d2), fn () => compare (r1, r2))
      | (KArrow _, _) => LESS
      | (_, KArrow _) => GREATER

      | (KName, KName) => EQUAL
      | (KName, _) => LESS
      | (_, KName) => GREATER

      | (KRecord k1, KRecord k2) => compare (k1, k2)
      | (KRecord _, _) => LESS
      | (_, KRecord _) => GREATER

      | (KUnit, KUnit) => EQUAL
      | (KUnit, _) => LESS
      | (_, KUnit) => GREATER

      | (KTuple ks1, KTuple ks2) => joinL compare (ks1, ks2)
      | (KTuple _, _) => LESS
      | (_, KTuple _) => GREATER

      | (KRel n1, KRel n2) => Int.compare (n1, n2)
      | (KRel _, _) => LESS
      | (_, KRel _) => GREATER

      | (KFun (_, k1), KFun (_, k2)) => compare (k1, k2)

fun mapfoldB {kind = f, bind} =
    let
        fun mfk ctx k acc =
            S.bindP (mfk' ctx k acc, f ctx)

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

              | KRel _ => S.return2 kAll
              | KFun (x, k) =>
                S.map2 (mfk (bind (ctx, x)) k,
                        fn k' =>
                           (KFun (x, k'), loc))
    in
        mfk
    end

fun mapfold fk =
    mapfoldB {kind = fn () => fk,
              bind = fn ((), _) => ()} ()

fun map f k =
    case mapfold (fn k => fn () => S.Continue (f k, ())) k () of
        S.Return () => raise Fail "CoreUtil.Kind.map"
      | S.Continue (k, ()) => k

fun mapB {kind, bind} ctx k =
    case mapfoldB {kind = fn ctx => fn k => fn () => S.Continue (kind ctx k, ()),
                   bind = bind} ctx k () of
        S.Continue (k, ()) => k
      | S.Return _ => raise Fail "CoreUtil.Kind.mapB: Impossible"

fun exists f k =
    case mapfold (fn k => fn () =>
                             if f k then
                                 S.Return ()
                             else
                                 S.Continue (k, ())) k () of
        S.Return _ => true
      | S.Continue _ => false

end

structure Con = struct

open Order

fun compare ((c1, _), (c2, _)) =
    case (c1, c2) of
        (TFun (d1, r1), TFun (d2, r2)) => join (compare (d1, d2), fn () => compare (r1, r2))
      | (TFun _, _) => LESS
      | (_, TFun _) => GREATER

      | (TCFun (x1, k1, r1), TCFun (x2, k2, r2)) =>
        join (String.compare (x1, x2),
           fn () => join (Kind.compare (k1, k2),
                          fn () => compare (r1, r2)))
      | (TCFun _, _) => LESS
      | (_, TCFun _) => GREATER

      | (TRecord c1, TRecord c2) => compare (c1, c2)
      | (TRecord _, _) => LESS
      | (_, TRecord _) => GREATER

      | (CRel n1, CRel n2) => Int.compare (n1, n2)
      | (CRel _, _) => LESS
      | (_, CRel _) => GREATER

      | (CNamed n1, CNamed n2) => Int.compare (n1, n2)
      | (CNamed _, _) => LESS
      | (_, CNamed _) => GREATER

      | (CFfi (m1, s1), CFfi (m2, s2)) => join (String.compare (m1, m2),
                                                fn () => String.compare (s1, s2))
      | (CFfi _, _) => LESS
      | (_, CFfi _) => GREATER

      | (CApp (f1, x1), CApp (f2, x2)) => join (compare (f1, f2),
                                                fn () => compare (x1, x2))
      | (CApp _, _) => LESS
      | (_, CApp _) => GREATER

      | (CAbs (x1, k1, b1), CAbs (x2, k2, b2)) =>
        join (String.compare (x1, x2),
              fn () => join (Kind.compare (k1, k2),
                             fn () => compare (b1, b2)))
      | (CAbs _, _) => LESS
      | (_, CAbs _) => GREATER

      | (CName s1, CName s2) => String.compare (s1, s2)
      | (CName _, _) => LESS
      | (_, CName _) => GREATER

      | (CRecord (k1, xvs1), CRecord (k2, xvs2)) =>
        join (Kind.compare (k1, k2),
              fn () =>
                 let
                     val sort = ListMergeSort.sort (fn ((x1, _), (x2, _)) =>
                                                       compare (x1, x2) = GREATER)
                 in
                     joinL (fn ((x1, v1), (x2, v2)) =>
                               join (compare (x1, x2),
                                  fn () => compare (v1, v2))) (sort xvs1, sort xvs2)
                 end)
      | (CRecord _, _) => LESS
      | (_, CRecord _) => GREATER

      | (CConcat (f1, s1), CConcat (f2, s2)) =>
        join (compare (f1, f2),
              fn () => compare (s1, s2))
      | (CConcat _, _) => LESS
      | (_, CConcat _) => GREATER

      | (CMap (d1, r1), CMap (d2, r2)) =>
        join (Kind.compare (d1, d2),
              fn () => Kind.compare (r1, r2))
      | (CMap _, _) => LESS
      | (_, CMap _) => GREATER

      | (CUnit, CUnit) => EQUAL
      | (CUnit, _) => LESS
      | (_, CUnit) => GREATER

      | (CTuple cs1, CTuple cs2) => joinL compare (cs1, cs2)
      | (CTuple _, _) => LESS
      | (_, CTuple _) => GREATER

      | (CProj (c1, n1), CProj (c2, n2)) => join (Int.compare (n1, n2),
                                                  fn () => compare (c1, c2))
      | (CProj _, _) => LESS
      | (_, CProj _) => GREATER

      | (CKAbs (_, c1), CKAbs (_, c2)) => compare (c1, c2)
      | (CKAbs _, _) => LESS
      | (_, CKAbs _) => GREATER

      | (CKApp (c1, k1), CKApp (c2, k2)) =>
        join (compare (c1, c2),
              fn () => Kind.compare (k1, k2))
      | (CKApp _, _) => LESS
      | (_, CKApp _) => GREATER

      | (TKFun (_, c1), TKFun (_, c2)) => compare (c1, c2)

datatype binder =
         RelK of string
       | RelC of string * kind
       | NamedC of string * int * kind * con option

fun mapfoldB {kind = fk, con = fc, bind} =
    let
        val mfk = Kind.mapfoldB {kind = fk, bind = fn (ctx, x) => bind (ctx, RelK x)}

        fun mfc ctx c acc =
            S.bindP (mfc' ctx c acc, fc ctx)

        and mfc' ctx (cAll as (c, loc)) =
            case c of
                TFun (c1, c2) =>
                S.bind2 (mfc ctx c1,
                      fn c1' =>
                         S.map2 (mfc ctx c2,
                              fn c2' =>
                                 (TFun (c1', c2'), loc)))
              | TCFun (x, k, c) =>
                S.bind2 (mfk ctx k,
                      fn k' =>
                         S.map2 (mfc (bind (ctx, RelC (x, k))) c,
                              fn c' =>
                                 (TCFun (x, k', c'), loc)))
              | TRecord c =>
                S.map2 (mfc ctx c,
                        fn c' =>
                           (TRecord c', loc))

              | CRel _ => S.return2 cAll
              | CNamed _ => S.return2 cAll
              | CFfi _ => S.return2 cAll
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

fun mapfold {kind = fk, con = fc} =
    mapfoldB {kind = fn () => fk,
              con = fn () => fc,
              bind = fn ((), _) => ()} ()

fun map {kind, con} c =
    case mapfold {kind = fn k => fn () => S.Continue (kind k, ()),
                  con = fn c => fn () => S.Continue (con c, ())} c () of
        S.Return () => raise Fail "Core_util.Con.map"
      | S.Continue (c, ()) => c

fun mapB {kind, con, bind} ctx c =
    case mapfoldB {kind = fn ctx => fn k => fn () => S.Continue (kind ctx k, ()),
                   con = fn ctx => fn c => fn () => S.Continue (con ctx c, ()),
                   bind = bind} ctx c () of
        S.Continue (c, ()) => c
      | S.Return _ => raise Fail "CoreUtil.Con.mapB: Impossible"

fun fold {kind, con} s c =
    case mapfold {kind = fn k => fn s => S.Continue (k, kind (k, s)),
                  con = fn c => fn s => S.Continue (c, con (c, s))} c s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "CoreUtil.Con.fold: Impossible"

fun exists {kind, con} k =
    case mapfold {kind = fn k => fn () =>
                                    if kind k then
                                        S.Return ()
                                    else
                                        S.Continue (k, ()),
                  con = fn c => fn () =>
                                    if con c then
                                        S.Return ()
                                    else
                                        S.Continue (c, ())} k () of
        S.Return _ => true
      | S.Continue _ => false

fun existsB {kind, con, bind} ctx c =
    case mapfoldB {kind = fn ctx => fn k => fn () =>
                                               if kind (ctx, k) then
                                                   S.Return ()
                                               else
                                                   S.Continue (k, ()),
                   con = fn ctx => fn c => fn () =>
                                              if con (ctx, c) then
                                                  S.Return ()
                                              else
                                                  S.Continue (c, ()),
                   bind = bind} ctx c () of
        S.Return _ => true
      | S.Continue _ => false

fun foldMap {kind, con} s c =
    case mapfold {kind = fn k => fn s => S.Continue (kind (k, s)),
                  con = fn c => fn s => S.Continue (con (c, s))} c s of
        S.Continue v => v
      | S.Return _ => raise Fail "CoreUtil.Con.foldMap: Impossible"

end

structure Exp = struct

open Order

fun pcCompare (pc1, pc2) =
    case (pc1, pc2) of
        (PConVar n1, PConVar n2) => Int.compare (n1, n2)
      | (PConVar _, _) => LESS
      | (_, PConVar _) => GREATER

      | (PConFfi {mod = m1, datatyp = d1, con = c1, ...},
         PConFfi {mod = m2, datatyp = d2, con = c2, ...}) =>
        join (String.compare (m1, m2),
              fn () => join (String.compare (d1, d2),
                             fn () => String.compare (c1, c2)))

fun pCompare ((p1, _), (p2, _)) =
    case (p1, p2) of
        (PVar _, PVar _) => EQUAL
      | (PVar _, _) => LESS
      | (_, PVar _) => GREATER

      | (PPrim p1, PPrim p2) => Prim.compare (p1, p2)
      | (PPrim _, _) => LESS
      | (_, PPrim _) => GREATER

      | (PCon (_, pc1, _, po1), PCon (_, pc2, _, po2)) =>
        join (pcCompare (pc1, pc2),
              fn () => joinO pCompare (po1, po2))
      | (PCon _, _) => LESS
      | (_, PCon _) => GREATER

      | (PRecord xps1, PRecord xps2) =>
        joinL (fn ((x1, p1, _), (x2, p2, _)) =>
                  join (String.compare (x1, x2),
                        fn () => pCompare (p1, p2))) (xps1, xps2)

fun fmCompare (fm1, fm2) =
    case (fm1, fm2) of
        (None, None) => EQUAL
      | (None, _) => LESS
      | (_, None) => GREATER

      | (Error, Error) => EQUAL

fun compare ((e1, _), (e2, _)) =
    case (e1, e2) of
        (EPrim p1, EPrim p2) => Prim.compare (p1, p2)
      | (EPrim _, _) => LESS
      | (_, EPrim _) => GREATER

      | (ERel n1, ERel n2) => Int.compare (n1, n2)
      | (ERel _, _) => LESS
      | (_, ERel _) => GREATER

      | (ENamed n1, ENamed n2) => Int.compare (n1, n2)
      | (ENamed _, _) => LESS
      | (_, ENamed _) => GREATER

      | (ECon (_, pc1, _, eo1), ECon (_, pc2, _, eo2)) =>
        join (pcCompare (pc1, pc2),
              fn () => joinO compare (eo1, eo2))
      | (ECon _, _) => LESS
      | (_, ECon _) => GREATER

      | (EFfi (f1, x1), EFfi (f2, x2)) =>
        join (String.compare (f1, f2),
              fn () => String.compare (x1, x2))
      | (EFfi _, _) => LESS
      | (_, EFfi _) => GREATER

      | (EFfiApp (f1, x1, es1), EFfiApp (f2, x2, es2)) =>
        join (String.compare (f1, f2),
           fn () => join (String.compare (x1, x2),
                       fn () => joinL (fn ((e1, _), (e2, _)) => compare (e1, e2))(es1, es2)))
      | (EFfiApp _, _) => LESS
      | (_, EFfiApp _) => GREATER

      | (EApp (f1, x1), EApp (f2, x2)) =>
        join (compare (f1, f2),
              fn () => compare (x1, x2))
      | (EApp _, _) => LESS
      | (_, EApp _) => GREATER

      | (EAbs (_, _, _, e1), EAbs (_, _, _, e2)) => compare (e1, e2)
      | (EAbs _, _) => LESS
      | (_, EAbs _) => GREATER

      | (ECApp (f1, x1), ECApp (f2, x2)) =>
        join (compare (f1, f2),
           fn () => Con.compare (x1, x2))
      | (ECApp _, _) => LESS
      | (_, ECApp _) => GREATER

      | (ECAbs (_, _, e1), ECAbs (_, _, e2)) => compare (e1, e2)
      | (ECAbs _, _) => LESS
      | (_, ECAbs _) => GREATER

      | (ERecord xes1, ERecord xes2) =>
        joinL (fn ((x1, e1, _), (x2, e2, _)) =>
                  join (Con.compare (x1, x2),
                        fn () => compare (e1, e2))) (xes1, xes2)
      | (ERecord _, _) => LESS
      | (_, ERecord _) => GREATER

      | (EField (e1, c1, _), EField (e2, c2, _)) =>
        join (compare (e1, e2),
              fn () => Con.compare (c1, c2))
      | (EField _, _) => LESS
      | (_, EField _) => GREATER

      | (EConcat (x1, _, y1, _), EConcat (x2, _, y2, _)) =>
        join (compare (x1, x2),
              fn () => compare (y1, y2))
      | (EConcat _, _) => LESS
      | (_, EConcat _) => GREATER

      | (ECut (e1, c1, _), ECut (e2, c2, _)) =>
        join (compare (e1, e2),
           fn () => Con.compare (c1, c2))
      | (ECut _, _) => LESS
      | (_, ECut _) => GREATER

      | (ECutMulti (e1, c1, _), ECutMulti (e2, c2, _)) =>
        join (compare (e1, e2),
           fn () => Con.compare (c1, c2))
      | (ECutMulti _, _) => LESS
      | (_, ECutMulti _) => GREATER

      | (ECase (e1, pes1, _), ECase (e2, pes2, _)) =>
        join (compare (e1, e2),
              fn () => joinL (fn ((p1, e1), (p2, e2)) =>
                                 join (pCompare (p1, p2),
                                       fn () => compare (e1, e2))) (pes1, pes2))
      | (ECase _, _) => LESS
      | (_, ECase _) => GREATER

      | (EWrite e1, EWrite e2) => compare (e1, e2)
      | (EWrite _, _) => LESS
      | (_, EWrite _) => GREATER

      | (EClosure (n1, es1), EClosure (n2, es2)) =>
        join (Int.compare (n1, n2),
              fn () => joinL compare (es1, es2))
      | (EClosure _, _) => LESS
      | (_, EClosure _) => GREATER

      | (ELet (_, _, x1, e1), ELet (_, _, x2, e2)) =>
        join (compare (x1, x2),
              fn () => compare (e1, e2))
      | (ELet _, _) => LESS
      | (_, ELet _) => GREATER

      | (EServerCall (n1, es1, _, fm1), EServerCall (n2, es2, _, fm2)) =>
        join (Int.compare (n1, n2),
           fn () => join (fmCompare (fm1, fm2),
                       fn () => joinL compare (es1, es2)))
      | (EServerCall _, _) => LESS
      | (_, EServerCall _) => GREATER

      | (EKAbs (_, e1), EKAbs (_, e2)) => compare (e1, e2)
      | (EKAbs _, _) => LESS
      | (_, EKAbs _) => GREATER

      | (EKApp (e1, k1), EKApp (e2, k2)) =>
        join (compare (e1, e2),
              fn () => Kind.compare (k1, k2))

datatype binder =
         RelK of string
       | RelC of string * kind
       | NamedC of string * int * kind * con option
       | RelE of string * con
       | NamedE of string * int * con * exp option * string

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

        fun mfe ctx e acc =
            S.bindP (mfe' ctx e acc, fe ctx)

        and mfet ctx (e, t) =
            S.bind2 (mfe ctx e,
                  fn e' =>
                     S.map2 (mfc ctx t,
                          fn t' => (e', t')))

        and mfe' ctx (eAll as (e, loc)) =
            case e of
                EPrim _ => S.return2 eAll
              | ERel _ => S.return2 eAll
              | ENamed _ => S.return2 eAll
              | ECon (dk, pc, cs, NONE) =>
                S.bind2 (mfpc ctx pc,
                      fn pc' =>
                         S.map2 (ListUtil.mapfold (mfc ctx) cs,
                              fn cs' =>
                                 (ECon (dk, pc', cs', NONE), loc)))
              | ECon (dk, pc, cs, SOME e) =>
                S.bind2 (mfpc ctx pc,
                      fn pc' =>
                         S.bind2 (mfe ctx e,
                               fn e' =>
                                  S.map2 (ListUtil.mapfold (mfc ctx) cs,
                                       fn cs' =>
                                          (ECon (dk, pc', cs', SOME e'), loc))))
              | EFfi _ => S.return2 eAll
              | EFfiApp (m, x, es) =>
                S.map2 (ListUtil.mapfold (mfet ctx) es,
                     fn es' =>
                        (EFfiApp (m, x, es'), loc))
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
              | ECAbs (x, k, e) =>
                S.bind2 (mfk ctx k,
                      fn k' =>
                         S.map2 (mfe (bind (ctx, RelC (x, k))) e,
                              fn e' =>
                                 (ECAbs (x, k', e'), loc)))

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

              | EWrite e =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (EWrite e', loc))

              | EClosure (n, es) =>
                S.map2 (ListUtil.mapfold (mfe ctx) es,
                     fn es' =>
                        (EClosure (n, es'), loc))

              | ELet (x, t, e1, e2) =>
                S.bind2 (mfc ctx t,
                         fn t' =>
                            S.bind2 (mfe ctx e1,
                                  fn e1' =>
                                     S.map2 (mfe (bind (ctx, RelE (x, t'))) e2,
                                          fn e2' =>
                                             (ELet (x, t', e1', e2'), loc))))

              | EServerCall (n, es, t, fm) =>
                S.bind2 (ListUtil.mapfold (mfe ctx) es,
                      fn es' =>
                         S.map2 (mfc ctx t,
                              fn t' =>
                                 (EServerCall (n, es', t', fm), loc)))

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
                S.bind2 (mfpc ctx pc,
                         fn pc' =>
                            S.bind2 (ListUtil.mapfold (mfc ctx) args,
                                     fn args' =>
                                        S.map2 ((case po of
                                                     NONE => S.return2 NONE
                                                   | SOME p => S.map2 (mfp ctx p, SOME)),
                                                fn po' =>
                                                   (PCon (dk, pc', args', po'), loc))))
              | PRecord xps =>
                S.map2 (ListUtil.mapfold (fn (x, p, c) =>
                                              S.bind2 (mfp ctx p,
                                                       fn p' =>
                                                          S.map2 (mfc ctx c,
                                                                  fn c' =>
                                                                     (x, p', c')))) xps,
                         fn xps' =>
                            (PRecord xps', loc))

        and mfpc ctx pc =
            case pc of
                PConVar _ => S.return2 pc
              | PConFfi {mod = m, datatyp, params, con, arg, kind} =>
                S.map2 ((case arg of
                             NONE => S.return2 NONE
                           | SOME c =>
                             let
                                 val k = (KType, ErrorMsg.dummySpan)
                                 val ctx' = foldl (fn (x, ctx) => bind (ctx, RelC (x, k))) ctx params
                             in
                                 S.map2 (mfc ctx' c, SOME)
                             end),
                        fn arg' =>
                           PConFfi {mod = m, datatyp = datatyp, params = params,
                                    con = con, arg = arg', kind = kind})
    in
        mfe
    end

fun mapfold {kind = fk, con = fc, exp = fe} =
    mapfoldB {kind = fn () => fk,
              con = fn () => fc,
              exp = fn () => fe,
              bind = fn ((), _) => ()} ()

fun mapB {kind, con, exp, bind} ctx e =
    case mapfoldB {kind = fn ctx => fn k => fn () => S.Continue (kind ctx k, ()),
                   con = fn ctx => fn c => fn () => S.Continue (con ctx c, ()),
                   exp = fn ctx => fn e => fn () => S.Continue (exp ctx e, ()),
                   bind = bind} ctx e () of
        S.Continue (e, ()) => e
      | S.Return _ => raise Fail "CoreUtil.Exp.mapB: Impossible"

fun map {kind, con, exp} e =
    case mapfold {kind = fn k => fn () => S.Continue (kind k, ()),
                  con = fn c => fn () => S.Continue (con c, ()),
                  exp = fn e => fn () => S.Continue (exp e, ())} e () of
        S.Return () => raise Fail "Core_util.Exp.map"
      | S.Continue (e, ()) => e

fun fold {kind, con, exp} s e =
    case mapfold {kind = fn k => fn s => S.Continue (k, kind (k, s)),
                  con = fn c => fn s => S.Continue (c, con (c, s)),
                  exp = fn e => fn s => S.Continue (e, exp (e, s))} e s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "CoreUtil.Exp.fold: Impossible"

fun foldB {kind, con, exp, bind} ctx s e =
    case mapfoldB {kind = fn ctx => fn k => fn s => S.Continue (k, kind (ctx, k, s)),
                  con = fn ctx => fn c => fn s => S.Continue (c, con (ctx, c, s)),
                  exp = fn ctx => fn e => fn s => S.Continue (e, exp (ctx, e, s)),
                  bind = bind} ctx e s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "CoreUtil.Exp.foldB: Impossible"

fun exists {kind, con, exp} k =
    case mapfold {kind = fn k => fn () =>
                                    if kind k then
                                        S.Return ()
                                    else
                                        S.Continue (k, ()),
                  con = fn c => fn () =>
                                    if con c then
                                        S.Return ()
                                    else
                                        S.Continue (c, ()),
                  exp = fn e => fn () =>
                                    if exp e then
                                        S.Return ()
                                    else
                                        S.Continue (e, ())} k () of
        S.Return _ => true
      | S.Continue _ => false

fun existsB {kind, con, exp, bind} ctx k =
    case mapfoldB {kind = fn ctx => fn k => fn () =>
                                               if kind (ctx, k) then
                                                   S.Return ()
                                               else
                                                   S.Continue (k, ()),
                   con = fn ctx => fn c => fn () =>
                                              if con (ctx, c) then
                                                  S.Return ()
                                              else
                                                  S.Continue (c, ()),
                   exp = fn ctx => fn e => fn () =>
                                              if exp (ctx, e) then
                                                  S.Return ()
                                              else
                                                  S.Continue (e, ()),
                   bind = bind} ctx k () of
        S.Return _ => true
      | S.Continue _ => false

fun foldMap {kind, con, exp} s e =
    case mapfold {kind = fn k => fn s => S.Continue (kind (k, s)),
                  con = fn c => fn s => S.Continue (con (c, s)),
                  exp = fn e => fn s => S.Continue (exp (e, s))} e s of
        S.Continue v => v
      | S.Return _ => raise Fail "CoreUtil.Exp.foldMap: Impossible"

fun foldMapB {kind, con, exp, bind} ctx s e =
    case mapfoldB {kind = fn ctx => fn k => fn s => S.Continue (kind (ctx, k, s)),
                   con = fn ctx => fn c => fn s => S.Continue (con (ctx, c, s)),
                   exp = fn ctx => fn e => fn s => S.Continue (exp (ctx, e, s)),
                   bind = bind} ctx e s of
        S.Continue v => v
      | S.Return _ => raise Fail "CoreUtil.Exp.foldMapB: Impossible"

end

structure Decl = struct

datatype binder = datatype Exp.binder

fun mapfoldB {kind = fk, con = fc, exp = fe, decl = fd, bind} =
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

        val mfe = Exp.mapfoldB {kind = fk, con = fc, exp = fe, bind = bind}

        fun mfd ctx d acc =
            S.bindP (mfd' ctx d acc, fd ctx)

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
                                             let
                                                 val k = (KType, loc)
                                                 val k' = foldl (fn (_, k') => (KArrow (k, k'), loc)) k xs
                                                 val ctx' = bind (ctx, NamedC (x, n, k', NONE))
                                             in
                                                 S.map2 (ListUtil.mapfold (fn (x, n, c) =>
                                                                              case c of
                                                                                  NONE => S.return2 (x, n, c)
                                                                                | SOME c =>
                                                                                  S.map2 (mfc ctx' c,
                                                                                       fn c' => (x, n, SOME c'))) xncs,
                                                         fn xncs' => (x, n, xs, xncs'))
                                             end) dts,
                     fn dts' =>
                        (DDatatype dts', loc))
              | DVal vi =>
                S.map2 (mfvi ctx vi,
                     fn vi' =>
                        (DVal vi', loc))
              | DValRec vis =>
                let
                    val ctx = foldl (fn ((x, n, t, e, s), ctx) => bind (ctx, NamedE (x, n, t, NONE, s)))
                                    ctx vis
                in
                    S.map2 (ListUtil.mapfold (mfvi ctx) vis,
                         fn vis' =>
                            (DValRec vis', loc))
                end
              | DExport _ => S.return2 dAll
              | DTable (x, n, c, s, pe, pc, ce, cc) =>
                let
                    val loc = #2 ce
                    val ct = (CFfi ("Basis", "sql_table"), loc)
                    val ct = (CApp (ct, (CConcat (pc, cc), loc)), loc)
                    val ct = (CApp (ct, cc), loc)
                    val ctx' = bind (ctx, NamedE (x, n, ct, NONE, s))
                in
                    S.bind2 (mfc ctx c,
                          fn c' =>
                             S.bind2 (mfe ctx' pe,
                                   fn pe' =>
                                      S.bind2 (mfc ctx pc,
                                            fn pc' =>
                                               S.bind2 (mfe ctx' ce,
                                                     fn ce' =>
                                                        S.map2 (mfc ctx cc,
                                                             fn cc' =>
                                                                (DTable (x, n, c', s, pe', pc', ce', cc'), loc))))))
                end
              | DSequence _ => S.return2 dAll
              | DView (x, n, s, e, c) =>
                S.bind2 (mfe ctx e,
                     fn e' =>
                        S.map2 (mfc ctx c,
                             fn c' =>
                                (DView (x, n, s, e', c'), loc)))
              | DIndex (e1, e2) =>
                S.bind2 (mfe ctx e1,
                     fn e1' =>
                        S.map2 (mfe ctx e2,
                             fn e2' =>
                                (DIndex (e1', e2'), loc)))
              | DDatabase _ => S.return2 dAll
              | DCookie (x, n, c, s) =>
                S.map2 (mfc ctx c,
                     fn c' =>
                        (DCookie (x, n, c', s), loc))
              | DStyle _ => S.return2 dAll
              | DTask (e1, e2) =>
                S.bind2 (mfe ctx e1,
                     fn e1' =>
                        S.map2 (mfe ctx e2,
                             fn e2' =>
                                (DTask (e1', e2'), loc)))
              | DPolicy e =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (DPolicy e', loc))

              | DOnError _ => S.return2 dAll

        and mfvi ctx (x, n, t, e, s) =
            S.bind2 (mfc ctx t,
                  fn t' =>
                     S.map2 (mfe ctx e,
                          fn e' =>
                             (x, n, t', e', s)))
    in
        mfd
    end    

fun mapfold {kind = fk, con = fc, exp = fe, decl = fd} =
    mapfoldB {kind = fn () => fk,
              con = fn () => fc,
              exp = fn () => fe,
              decl = fn () => fd,
              bind = fn ((), _) => ()} ()

fun mapB {kind, con, exp, decl, bind} ctx d =
    case mapfoldB {kind = fn ctx => fn k => fn () => S.Continue (kind ctx k, ()),
                   con = fn ctx => fn c => fn () => S.Continue (con ctx c, ()),
                   exp = fn ctx => fn e => fn () => S.Continue (exp ctx e, ()),
                   decl = fn ctx => fn d => fn () => S.Continue (decl ctx d, ()),
                   bind = bind} ctx d () of
        S.Continue (d, ()) => d
      | S.Return _ => raise Fail "CoreUtil.Decl.mapB: Impossible"

fun map {kind, con, exp, decl} d =
    mapB {kind = fn () => kind,
          con = fn () => con,
          exp = fn () => exp,
          decl = fn () => decl,
          bind = fn _ => ()} () d
             
fun fold {kind, con, exp, decl} s d =
    case mapfold {kind = fn k => fn s => S.Continue (k, kind (k, s)),
                  con = fn c => fn s => S.Continue (c, con (c, s)),
                  exp = fn e => fn s => S.Continue (e, exp (e, s)),
                  decl = fn d => fn s => S.Continue (d, decl (d, s))} d s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "CoreUtil.Decl.fold: Impossible"

fun foldMap {kind, con, exp, decl} s d =
    case mapfold {kind = fn k => fn s => S.Continue (kind (k, s)),
                  con = fn c => fn s => S.Continue (con (c, s)),
                  exp = fn e => fn s => S.Continue (exp (e, s)),
                  decl = fn d => fn s => S.Continue (decl (d, s))} d s of
        S.Continue v => v
      | S.Return _ => raise Fail "CoreUtil.Decl.foldMap: Impossible"

fun foldMapB {kind, con, exp, decl, bind} ctx s d =
    case mapfoldB {kind = fn ctx => fn k => fn s => S.Continue (kind (ctx, k, s)),
                   con = fn ctx => fn c => fn s => S.Continue (con (ctx, c, s)),
                   exp = fn ctx => fn e => fn s => S.Continue (exp (ctx, e, s)),
                   decl = fn ctx => fn d => fn s => S.Continue (decl (ctx, d, s)),
                   bind = bind} ctx d s of
        S.Continue v => v
      | S.Return _ => raise Fail "CoreUtil.Decl.foldMapB: Impossible"

fun exists {kind, con, exp, decl} d =
    case mapfold {kind = fn k => fn () =>
                                    if kind k then
                                        S.Return ()
                                    else
                                        S.Continue (k, ()),
                  con = fn c => fn () =>
                                    if con c then
                                        S.Return ()
                                    else
                                        S.Continue (c, ()),
                  exp = fn e => fn () =>
                                    if exp e then
                                        S.Return ()
                                    else
                                        S.Continue (e, ()),
                  decl = fn d => fn () =>
                                   if decl d then
                                       S.Return ()
                                   else
                                       S.Continue (d, ())} d () of
        S.Return _ => true
      | S.Continue _ => false

end

structure File = struct

datatype binder = datatype Exp.binder

fun mapfoldB (all as {bind, ...}) =
    let
        val mfd = Decl.mapfoldB all

        fun mff ctx ds =
            case ds of
                nil => S.return2 nil
              | d :: ds' =>
                S.bind2 (mfd ctx d,
                         fn d' =>
                            let
                                val ctx' =
                                    case #1 d' of
                                        DCon (x, n, k, c) => bind (ctx, NamedC (x, n, k, SOME c))
                                      | DDatatype dts =>
                                        foldl (fn ((x, n, xs, xncs), ctx) =>
                                                  let
                                                      val loc = #2 d'
                                                      val k = (KType, loc)
                                                      val k' = foldl (fn (_, k') => (KArrow (k, k'), loc)) k xs

                                                      val ctx = bind (ctx, NamedC (x, n, k', NONE))
                                                      val t = (CNamed n, #2 d')
                                                      val t = ListUtil.foldli (fn (i, _, t) =>
                                                                                  (CApp (t, (CRel i, loc)), loc))
                                                                              t xs
                                                  in
                                                      foldl (fn ((x, n, to), ctx) =>
                                                                let
                                                                    val t = case to of
                                                                                NONE => t
                                                                              | SOME t' => (TFun (t', t), #2 d')
                                                                    val t = foldr (fn (x, t) => (TCFun (x, k, t), loc))
                                                                                  t xs
                                                                in
                                                                    bind (ctx, NamedE (x, n, t, NONE, ""))
                                                                end)
                                                            ctx xncs
                                                  end)
                                        ctx dts
                                      | DVal (x, n, t, e, s) => bind (ctx, NamedE (x, n, t, SOME e, s))
                                      | DValRec vis =>
                                        foldl (fn ((x, n, t, e, s), ctx) => bind (ctx, NamedE (x, n, t, NONE, s)))
                                        ctx vis
                                      | DExport _ => ctx
                                      | DTable (x, n, c, s, _, pc, _, cc) =>
                                        let
                                            val loc = #2 d'
                                            val ct = (CFfi ("Basis", "sql_table"), loc)
                                            val ct = (CApp (ct, (CConcat (pc, cc), loc)), loc)
                                            val ct = (CApp (ct, cc), loc)
                                        in
                                            bind (ctx, NamedE (x, n, ct, NONE, s))
                                        end
                                      | DSequence (x, n, s) =>
                                        let
                                            val t = (CFfi ("Basis", "sql_sequence"), #2 d')
                                        in
                                            bind (ctx, NamedE (x, n, t, NONE, s))
                                        end
                                      | DView (x, n, s, _, c) =>
                                        let
                                            val loc = #2 d'
                                            val ct = (CFfi ("Basis", "sql_view"), loc)
                                            val ct = (CApp (ct, c), loc)
                                        in
                                            bind (ctx, NamedE (x, n, ct, NONE, s))
                                        end
                                      | DIndex _ => ctx
                                      | DDatabase _ => ctx
                                      | DCookie (x, n, c, s) =>
                                        let
                                            val t = (CApp ((CFfi ("Basis", "http_cookie"), #2 d'), c), #2 d')
                                        in
                                            bind (ctx, NamedE (x, n, t, NONE, s))
                                        end
                                      | DStyle (x, n, s) =>
                                        let
                                            val t = (CFfi ("Basis", "css_class"), #2 d')
                                        in
                                            bind (ctx, NamedE (x, n, t, NONE, s))
                                        end
                                      | DTask _ => ctx
                                      | DPolicy _ => ctx
                                      | DOnError _ => ctx
                            in
                                S.map2 (mff ctx' ds',
                                     fn ds' =>
                                        d' :: ds')
                            end)
    in
        mff
    end

fun mapfold {kind = fk, con = fc, exp = fe, decl = fd} =
    mapfoldB {kind = fn () => fk,
              con = fn () => fc,
              exp = fn () => fe,
              decl = fn () => fd,
              bind = fn ((), _) => ()} ()

fun mapB {kind, con, exp, decl, bind} ctx ds =
    case mapfoldB {kind = fn ctx => fn k => fn () => S.Continue (kind ctx k, ()),
                   con = fn ctx => fn c => fn () => S.Continue (con ctx c, ()),
                   exp = fn ctx => fn e => fn () => S.Continue (exp ctx e, ()),
                   decl = fn ctx => fn d => fn () => S.Continue (decl ctx d, ()),
                   bind = bind} ctx ds () of
        S.Continue (ds, ()) => ds
      | S.Return _ => raise Fail "CoreUtil.File.mapB: Impossible"

fun map {kind, con, exp, decl} ds =
    mapB {kind = fn () => kind,
          con = fn () => con,
          exp = fn () => exp,
          decl = fn () => decl,
          bind = fn _ => ()} () ds

fun fold {kind, con, exp, decl} s d =
    case mapfold {kind = fn k => fn s => S.Continue (k, kind (k, s)),
                  con = fn c => fn s => S.Continue (c, con (c, s)),
                  exp = fn e => fn s => S.Continue (e, exp (e, s)),
                  decl = fn d => fn s => S.Continue (d, decl (d, s))} d s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "CoreUtil.File.fold: Impossible"

fun foldMap {kind, con, exp, decl} s d =
    case mapfold {kind = fn k => fn s => S.Continue (kind (k, s)),
                  con = fn c => fn s => S.Continue (con (c, s)),
                  exp = fn e => fn s => S.Continue (exp (e, s)),
                  decl = fn d => fn s => S.Continue (decl (d, s))} d s of
        S.Continue v => v
      | S.Return _ => raise Fail "CoreUtil.File.foldMap: Impossible"

val maxName = foldl (fn ((d, _) : decl, count) =>
                        case d of
                            DCon (_, n, _, _) => Int.max (n, count)
                          | DDatatype dts => foldl (fn ((_, n, _, ns), count) =>
                                                       foldl (fn ((_, n', _), m) => Int.max (n', m))
                                                             (Int.max (n, count)) ns) count dts
                          | DVal (_, n, _, _, _) => Int.max (n, count)
                          | DValRec vis => foldl (fn ((_, n, _, _, _), count) => Int.max (n, count)) count vis
                          | DExport _ => count
                          | DTable (_, n, _, _, _, _, _, _) => Int.max (n, count)
                          | DSequence (_, n, _) => Int.max (n, count)
                          | DView (_, n, _, _, _) => Int.max (n, count)
                          | DIndex _ => count
                          | DDatabase _ => count
                          | DCookie (_, n, _, _) => Int.max (n, count)
                          | DStyle (_, n, _) => Int.max (n, count)
                          | DTask _ => count
                          | DPolicy _ => count
                          | DOnError _ => count) 0

end

end
