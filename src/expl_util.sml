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

structure ExplUtil :> EXPL_UTIL = struct

open Expl

structure S = Search

structure Kind = struct

fun mapfoldB {kind, bind} =
    let
        fun mfk ctx k acc =
            S.bindP (mfk' ctx k acc, kind ctx)

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

fun mapB {kind, bind} ctx k =
    case mapfoldB {kind = fn ctx => fn k => fn () => S.Continue (kind ctx k, ()),
                   bind = bind} ctx k () of
        S.Continue (k, ()) => k
      | S.Return _ => raise Fail "ExplUtil.Kind.mapB: Impossible"

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

datatype binder =
         RelK of string
       | RelC of string * Expl.kind
       | NamedC of string * Expl.kind

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

fun mapB {kind, con, bind} ctx c =
    case mapfoldB {kind = fn ctx => fn k => fn () => S.Continue (kind ctx k, ()),
                   con = fn ctx => fn c => fn () => S.Continue (con ctx c, ()),
                   bind = bind} ctx c () of
        S.Continue (c, ()) => c
      | S.Return _ => raise Fail "ExplUtil.Con.mapB: Impossible"

fun map {kind, con} s =
    case mapfold {kind = fn k => fn () => S.Continue (kind k, ()),
                  con = fn c => fn () => S.Continue (con c, ())} s () of
        S.Return () => raise Fail "ExplUtil.Con.map: Impossible"
      | S.Continue (s, ()) => s

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

end

structure Exp = struct

datatype binder =
         RelK of string
       | RelC of string * Expl.kind
       | NamedC of string * Expl.kind
       | RelE of string * Expl.con
       | NamedE of string * Expl.con

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

              | EWrite e =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (EWrite e', loc))

              | ECase (e, pes, {disc, result}) =>
                S.bind2 (mfe ctx e,
                         fn e' =>
                            S.bind2 (ListUtil.mapfold (fn (p, e) =>
                                                         S.map2 (mfe ctx e,
                                                              fn e' => (p, e'))) pes,
                                    fn pes' =>
                                       S.bind2 (mfc ctx disc,
                                                fn disc' =>
                                                   S.map2 (mfc ctx result,
                                                        fn result' =>
                                                           (ECase (e', pes', {disc = disc', result = result'}), loc)))))

              | ELet (x, t, e1, e2) =>
                S.bind2 (mfc ctx t,
                         fn t' =>
                            S.bind2 (mfe ctx e1,
                                  fn e1' =>
                                     S.map2 (mfe (bind (ctx, RelE (x, t))) e2,
                                          fn e2' =>
                                             (ELet (x, t', e1', e2'), loc))))

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
    in
        mfe
    end

fun mapfold {kind = fk, con = fc, exp = fe} =
    mapfoldB {kind = fn () => fk,
              con = fn () => fc,
              exp = fn () => fe,
              bind = fn ((), _) => ()} ()

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

end

structure Sgn = struct

datatype binder =
         RelK of string
       | RelC of string * Expl.kind
       | NamedC of string * Expl.kind
       | Str of string * Expl.sgn
       | Sgn of string * Expl.sgn

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
            S.bindP (sgi' ctx si acc, sgn_item ctx)

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
              | SgiStr (x, n, s) =>
                S.map2 (sg ctx s,
                     fn s' =>
                        (SgiStr (x, n, s'), loc))
              | SgiSgn (x, n, s) =>
                S.map2 (sg ctx s,
                     fn s' =>
                        (SgiSgn (x, n, s'), loc))

        and sg ctx s acc =
            S.bindP (sg' ctx s acc, sgn ctx)

        and sg' ctx (sAll as (s, loc)) =
            case s of
                SgnConst sgis =>
                S.map2 (ListUtil.mapfoldB (fn (ctx, si)  =>
                                              (case #1 si of
                                                   SgiConAbs (x, _, k) =>
                                                   bind (ctx, NamedC (x, k))
                                                 | SgiCon (x, _, k, _) =>
                                                   bind (ctx, NamedC (x, k))
                                                 | SgiDatatype dts =>
                                                   foldl (fn ((x, _, ks, _), ctx) =>
                                                             let
                                                                 val k' = (KType, loc)
                                                                 val k = foldl (fn (_, k) => (KArrow (k', k), loc))
                                                                               k' ks
                                                             in
                                                                 bind (ctx, NamedC (x, k))
                                                             end) ctx dts
                                                 | SgiDatatypeImp (x, _, _, _, _, _, _) =>
                                                   bind (ctx, NamedC (x, (KType, loc)))
                                                 | SgiVal _ => ctx
                                                 | SgiStr (x, _, sgn) =>
                                                   bind (ctx, Str (x, sgn))
                                                 | SgiSgn (x, _, sgn) =>
                                                   bind (ctx, Sgn (x, sgn)),
                                               sgi ctx si)) ctx sgis,
                     fn sgis' =>
                        (SgnConst sgis', loc))
                
              | SgnVar _ => S.return2 sAll

              | SgnFun (m, n, s1, s2) =>
                S.bind2 (sg ctx s1,
                         fn s1' =>
                            S.map2 (sg (bind (ctx, Str (m, s1'))) s2,
                                    fn s2' =>
                                       (SgnFun (m, n, s1', s2'), loc)))
              | SgnWhere (sgn, x, c) =>
                S.bind2 (sg ctx sgn,
                      fn sgn' =>
                         S.map2 (con ctx c,
                              fn c' =>
                                 (SgnWhere (sgn', x, c'), loc)))
              | SgnProj _ => S.return2 sAll
    in
        sg
    end

fun mapfold {kind, con, sgn_item, sgn} =
    mapfoldB {kind = fn () => kind,
              con = fn () => con,
              sgn_item = fn () => sgn_item,
              sgn = fn () => sgn,
              bind = fn ((), _) => ()} ()

fun map {kind, con, sgn_item, sgn} s =
    case mapfold {kind = fn k => fn () => S.Continue (kind k, ()),
                  con = fn c => fn () => S.Continue (con c, ()),
                  sgn_item = fn si => fn () => S.Continue (sgn_item si, ()),
                  sgn = fn s => fn () => S.Continue (sgn s, ())} s () of
        S.Return () => raise Fail "Expl_util.Sgn.map"
      | S.Continue (s, ()) => s

end

end
