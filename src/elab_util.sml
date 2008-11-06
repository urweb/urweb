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

structure ElabUtil :> ELAB_UTIL = struct

open Elab

fun classifyDatatype xncs =
    case xncs of
        [(_, _, NONE), (_, _, SOME _)] => Option
      | [(_, _, SOME _), (_, _, NONE)] => Option
      | _ =>
        if List.all (fn (_, _, NONE) => true | _ => false) xncs then
            Enum
        else
            Default

structure S = Search

structure Kind = struct

fun mapfold f =
    let
        fun mfk k acc =
            S.bindP (mfk' k acc, f)

        and mfk' (kAll as (k, loc)) =
            case k of
                KType => S.return2 kAll

              | KArrow (k1, k2) =>
                S.bind2 (mfk k1,
                      fn k1' =>
                         S.map2 (mfk k2,
                              fn k2' =>
                                 (KArrow (k1', k2'), loc)))

              | KName => S.return2 kAll

              | KRecord k =>
                S.map2 (mfk k,
                        fn k' =>
                           (KRecord k', loc))

              | KUnit => S.return2 kAll

              | KTuple ks =>
                S.map2 (ListUtil.mapfold mfk ks,
                        fn ks' =>
                           (KTuple ks', loc))

              | KError => S.return2 kAll

              | KUnif (_, _, ref (SOME k)) => mfk' k
              | KUnif _ => S.return2 kAll
    in
        mfk
    end

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
         Rel of string * Elab.kind
       | Named of string * int * Elab.kind

fun mapfoldB {kind = fk, con = fc, bind} =
    let
        val mfk = Kind.mapfold fk

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
              | TCFun (e, x, k, c) =>
                S.bind2 (mfk k,
                      fn k' =>
                         S.map2 (mfc (bind (ctx, Rel (x, k))) c,
                              fn c' =>
                                 (TCFun (e, x, k', c'), loc)))
              | CDisjoint (ai, c1, c2, c3) =>
                S.bind2 (mfc ctx c1,
                      fn c1' =>
                         S.bind2 (mfc ctx c2,
                              fn c2' =>
                                 S.map2 (mfc ctx c3,
                                         fn c3' =>
                                            (CDisjoint (ai, c1', c2', c3'), loc))))
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
                S.bind2 (mfk k,
                      fn k' =>
                         S.map2 (mfc (bind (ctx, Rel (x, k))) c,
                              fn c' =>
                                 (CAbs (x, k', c'), loc)))

              | CName _ => S.return2 cAll

              | CRecord (k, xcs) =>
                S.bind2 (mfk k,
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
              | CFold (k1, k2) =>
                S.bind2 (mfk k1,
                         fn k1' =>
                            S.map2 (mfk k2,
                                    fn k2' =>
                                       (CFold (k1', k2'), loc)))

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
              | CUnif (_, _, _, ref (SOME c)) => mfc' ctx c
              | CUnif _ => S.return2 cAll
    in
        mfc
    end

fun mapfold {kind = fk, con = fc} =
    mapfoldB {kind = fk,
              con = fn () => fc,
              bind = fn ((), _) => ()} ()

fun mapB {kind, con, bind} ctx c =
    case mapfoldB {kind = fn k => fn () => S.Continue (kind k, ()),
                   con = fn ctx => fn c => fn () => S.Continue (con ctx c, ()),
                   bind = bind} ctx c () of
        S.Continue (c, ()) => c
      | S.Return _ => raise Fail "ElabUtil.Con.mapB: Impossible"

fun map {kind, con} s =
    case mapfold {kind = fn k => fn () => S.Continue (kind k, ()),
                  con = fn c => fn () => S.Continue (con c, ())} s () of
        S.Return () => raise Fail "ElabUtil.Con.map: Impossible"
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

fun foldB {kind, con, bind} ctx st c =
    case mapfoldB {kind = fn k => fn st => S.Continue (k, kind (k, st)),
                   con = fn ctx => fn c => fn st => S.Continue (c, con (ctx, c, st)),
                   bind = bind} ctx c st of
        S.Continue (_, st) => st
      | S.Return _ => raise Fail "ElabUtil.Con.foldB: Impossible"

end

structure Exp = struct

datatype binder =
         RelC of string * Elab.kind
       | NamedC of string * int * Elab.kind
       | RelE of string * Elab.con
       | NamedE of string * Elab.con

fun mapfoldB {kind = fk, con = fc, exp = fe, bind} =
    let
        val mfk = Kind.mapfold fk

        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Con.Rel x => RelC x
                           | Con.Named x => NamedC x
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
              | ECAbs (expl, x, k, e) =>
                S.bind2 (mfk k,
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

              | EFold k =>
                S.map2 (mfk k,
                         fn k' =>
                            (EFold k', loc))

              | ECase (e, pes, {disc, result}) =>
                S.bind2 (mfe ctx e,
                         fn e' =>
                            S.bind2 (ListUtil.mapfold (fn (p, e) =>
                                                          let
                                                              fun pb ((p, _), ctx) =
                                                                  case p of
                                                                      PWild => ctx
                                                                    | PVar (x, t) => bind (ctx, RelE (x, t))
                                                                    | PPrim _ => ctx
                                                                    | PCon (_, _, _, NONE) => ctx
                                                                    | PCon (_, _, _, SOME p) => pb (p, ctx)
                                                                    | PRecord xps => foldl (fn ((_, p, _), ctx) =>
                                                                                               pb (p, ctx)) ctx xps
                                                          in
                                                              S.map2 (mfe (pb (p, ctx)) e,
                                                                   fn e' => (p, e'))
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

              | ELet (des, e) =>
                let
                    val (des, ctx) = foldl (fn (ed, (des, ctx)) =>
                                               let
                                                   val ctx' =
                                                       case #1 ed of
                                                           EDVal (x, t, _) => bind (ctx, RelE (x, t))
                                                         | EDValRec vis =>
                                                           foldl (fn ((x, t, _), ctx) => bind (ctx, RelE (x, t))) ctx vis
                                               in
                                                   (S.bind2 (des,
                                                          fn des' =>
                                                             S.map2 (mfed ctx ed,
                                                               fn ed' => des' @ [ed'])),
                                                    ctx')
                                               end)
                                            (S.return2 [], ctx) des
                in
                    S.bind2 (des,
                         fn des' =>
                            S.map2 (mfe ctx e,
                                    fn e' =>
                                       (ELet (des', e'), loc)))
                end

        and mfed ctx (dAll as (d, loc)) =
            case d of
                EDVal vi =>
                S.map2 (mfvi ctx vi,
                     fn vi' =>
                        (EDVal vi', loc))
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

fun mapfold {kind = fk, con = fc, exp = fe} =
    mapfoldB {kind = fk,
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

fun mapB {kind, con, exp, bind} ctx e =
    case mapfoldB {kind = fn k => fn () => S.Continue (kind k, ()),
                   con = fn ctx => fn c => fn () => S.Continue (con ctx c, ()),
                   exp = fn ctx => fn e => fn () => S.Continue (exp ctx e, ()),
                   bind = bind} ctx e () of
        S.Continue (e, ()) => e
      | S.Return _ => raise Fail "ElabUtil.Exp.mapB: Impossible"

fun foldB {kind, con, exp, bind} ctx st e =
    case mapfoldB {kind = fn k => fn st => S.Continue (k, kind (k, st)),
                   con = fn ctx => fn c => fn st => S.Continue (c, con (ctx, c, st)),
                   exp = fn ctx => fn e => fn st => S.Continue (e, exp (ctx, e, st)),
                   bind = bind} ctx e st of
        S.Continue (_, st) => st
      | S.Return _ => raise Fail "ElabUtil.Exp.foldB: Impossible"

end

structure Sgn = struct

datatype binder =
         RelC of string * Elab.kind
       | NamedC of string * int * Elab.kind
       | Str of string * Elab.sgn
       | Sgn of string * Elab.sgn

fun mapfoldB {kind, con, sgn_item, sgn, bind} =
    let
        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Con.Rel x => RelC x
                           | Con.Named x => NamedC x
            in
                bind (ctx, b')
            end
        val con = Con.mapfoldB {kind = kind, con = con, bind = bind'}

        val kind = Kind.mapfold kind

        fun sgi ctx si acc =
            S.bindP (sgi' ctx si acc, sgn_item ctx)

        and sgi' ctx (siAll as (si, loc)) =
            case si of
                SgiConAbs (x, n, k) =>
                S.map2 (kind k,
                     fn k' =>
                        (SgiConAbs (x, n, k'), loc))
              | SgiCon (x, n, k, c) =>
                S.bind2 (kind k,
                     fn k' =>
                        S.map2 (con ctx c,
                             fn c' =>
                                (SgiCon (x, n, k', c'), loc)))
              | SgiDatatype (x, n, xs, xncs) =>
                S.map2 (ListUtil.mapfold (fn (x, n, c) =>
                                             case c of
                                                 NONE => S.return2 (x, n, c)
                                               | SOME c =>
                                                 S.map2 (con ctx c,
                                                      fn c' => (x, n, SOME c'))) xncs,
                        fn xncs' =>
                           (SgiDatatype (x, n, xs, xncs'), loc))
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
              | SgiConstraint (c1, c2) =>
                S.bind2 (con ctx c1,
                         fn c1' =>
                            S.map2 (con ctx c2,
                                    fn c2' =>
                                       (SgiConstraint (c1', c2'), loc)))
              | SgiClassAbs _ => S.return2 siAll
              | SgiClass (x, n, c) =>
                S.map2 (con ctx c,
                        fn c' =>
                           (SgiClass (x, n, c'), loc))

        and sg ctx s acc =
            S.bindP (sg' ctx s acc, sgn ctx)

        and sg' ctx (sAll as (s, loc)) =
            case s of
                SgnConst sgis =>
                S.map2 (ListUtil.mapfoldB (fn (ctx, si)  =>
                                              (case #1 si of
                                                   SgiConAbs (x, n, k) =>
                                                   bind (ctx, NamedC (x, n, k))
                                                 | SgiCon (x, n, k, _) =>
                                                   bind (ctx, NamedC (x, n, k))
                                                 | SgiDatatype (x, n, _, xncs) =>
                                                   bind (ctx, NamedC (x, n, (KType, loc)))
                                                 | SgiDatatypeImp (x, n, _, _, _, _, _) =>
                                                   bind (ctx, NamedC (x, n, (KType, loc)))
                                                 | SgiVal _ => ctx
                                                 | SgiStr (x, _, sgn) =>
                                                   bind (ctx, Str (x, sgn))
                                                 | SgiSgn (x, _, sgn) =>
                                                   bind (ctx, Sgn (x, sgn))
                                                 | SgiConstraint _ => ctx
                                                 | SgiClassAbs (x, n) =>
                                                   bind (ctx, NamedC (x, n, (KArrow ((KType, loc), (KType, loc)), loc)))
                                                 | SgiClass (x, n, _) =>
                                                   bind (ctx, NamedC (x, n, (KArrow ((KType, loc), (KType, loc)), loc))),
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
              | SgnProj _ => S.return2 sAll
              | SgnWhere (sgn, x, c) =>
                S.bind2 (sg ctx sgn,
                      fn sgn' =>
                         S.map2 (con ctx c,
                              fn c' =>
                                 (SgnWhere (sgn', x, c'), loc)))
              | SgnError => S.return2 sAll
    in
        sg
    end

fun mapfold {kind, con, sgn_item, sgn} =
    mapfoldB {kind = kind,
              con = fn () => con,
              sgn_item = fn () => sgn_item,
              sgn = fn () => sgn,
              bind = fn ((), _) => ()} ()

fun map {kind, con, sgn_item, sgn} s =
    case mapfold {kind = fn k => fn () => S.Continue (kind k, ()),
                  con = fn c => fn () => S.Continue (con c, ()),
                  sgn_item = fn si => fn () => S.Continue (sgn_item si, ()),
                  sgn = fn s => fn () => S.Continue (sgn s, ())} s () of
        S.Return () => raise Fail "Elab_util.Sgn.map"
      | S.Continue (s, ()) => s

end

structure Decl = struct

datatype binder =
         RelC of string * Elab.kind
       | NamedC of string * int * Elab.kind
       | RelE of string * Elab.con
       | NamedE of string * Elab.con
       | Str of string * Elab.sgn
       | Sgn of string * Elab.sgn

fun mapfoldB {kind = fk, con = fc, exp = fe, sgn_item = fsgi, sgn = fsg, str = fst, decl = fd, bind} =
    let
        val mfk = Kind.mapfold fk

        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Con.Rel x => RelC x
                           | Con.Named x => NamedC x
            in
                bind (ctx, b')
            end
        val mfc = Con.mapfoldB {kind = fk, con = fc, bind = bind'}

        fun bind' (ctx, b) =
            let
                val b' = case b of
                             Exp.RelC x => RelC x
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
                             Sgn.RelC x => RelC x
                           | Sgn.NamedC x => NamedC x
                           | Sgn.Sgn x => Sgn x
                           | Sgn.Str x => Str x
            in
                bind (ctx, b')
            end
        val mfsg = Sgn.mapfoldB {kind = fk, con = fc, sgn_item = fsgi, sgn = fsg, bind = bind'}

        fun mfst ctx str acc =
            S.bindP (mfst' ctx str acc, fst ctx)

        and mfst' ctx (strAll as (str, loc)) =
            case str of
                StrConst ds => 
                S.map2 (ListUtil.mapfoldB (fn (ctx, d)  =>
                                              (case #1 d of
                                                   DCon (x, n, k, _) =>
                                                   bind (ctx, NamedC (x, n, k))
                                                 | DDatatype (x, n, xs, xncs) =>
                                                   let
                                                       val ctx = bind (ctx, NamedC (x, n, (KType, loc)))
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
                                                 | DDatatypeImp (x, n, m, ms, x', _, _) =>
                                                   bind (ctx, NamedC (x, n, (KType, loc)))
                                                 | DVal (x, _, c, _) =>
                                                   bind (ctx, NamedE (x, c))
                                                 | DValRec vis =>
                                                   foldl (fn ((x, _, c, _), ctx) => bind (ctx, NamedE (x, c))) ctx vis
                                                 | DSgn (x, _, sgn) =>
                                                   bind (ctx, Sgn (x, sgn))
                                                 | DStr (x, _, sgn, _) =>
                                                   bind (ctx, Str (x, sgn))
                                                 | DFfiStr (x, _, sgn) =>
                                                   bind (ctx, Str (x, sgn))
                                                 | DConstraint _ => ctx
                                                 | DExport _ => ctx
                                                 | DTable (tn, x, n, c) =>
                                                   bind (ctx, NamedE (x, (CApp ((CModProj (n, [], "sql_table"), loc),
                                                                                c), loc)))
                                                 | DSequence (tn, x, n) =>
                                                   bind (ctx, NamedE (x, (CModProj (n, [], "sql_sequence"), loc)))
                                                 | DClass (x, n, _) =>
                                                   bind (ctx, NamedC (x, n, (KArrow ((KType, loc), (KType, loc)), loc)))
                                                 | DDatabase _ => ctx
                                                 | DCookie (tn, x, n, c) =>
                                                   bind (ctx, NamedE (x, (CApp ((CModProj (n, [], "cookie"), loc),
                                                                                c), loc))),
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
            S.bindP (mfd' ctx d acc, fd ctx)

        and mfd' ctx (dAll as (d, loc)) =
            case d of
                DCon (x, n, k, c) =>
                S.bind2 (mfk k,
                         fn k' =>
                            S.map2 (mfc ctx c,
                                    fn c' =>
                                       (DCon (x, n, k', c'), loc)))
              | DDatatype (x, n, xs, xncs) =>
                S.map2 (ListUtil.mapfold (fn (x, n, c) =>
                                             case c of
                                                 NONE => S.return2 (x, n, c)
                                               | SOME c =>
                                                 S.map2 (mfc ctx c,
                                                      fn c' => (x, n, SOME c'))) xncs,
                        fn xncs' =>
                           (DDatatype (x, n, xs, xncs'), loc))
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

              | DTable (tn, x, n, c) =>
                S.map2 (mfc ctx c,
                        fn c' =>
                           (DTable (tn, x, n, c'), loc))
              | DSequence _ => S.return2 dAll

              | DClass (x, n, c) =>
                S.map2 (mfc ctx c,
                     fn c' =>
                        (DClass (x, n, c'), loc))

              | DDatabase _ => S.return2 dAll

              | DCookie (tn, x, n, c) =>
                S.map2 (mfc ctx c,
                        fn c' =>
                           (DCookie (tn, x, n, c'), loc))

        and mfvi ctx (x, n, c, e) =
            S.bind2 (mfc ctx c,
                  fn c' =>
                     S.map2 (mfe ctx e,
                          fn e' =>
                             (x, n, c', e')))
    in
        mfd
    end

fun mapfold {kind, con, exp, sgn_item, sgn, str, decl} =
    mapfoldB {kind = kind,
              con = fn () => con,
              exp = fn () => exp,
              sgn_item = fn () => sgn_item,
              sgn = fn () => sgn,
              str = fn () => str,
              decl = fn () => decl,
              bind = fn ((), _) => ()} ()

fun exists {kind, con, exp, sgn_item, sgn, str, decl} k =
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
                  sgn_item = fn sgi => fn () =>
                                        if sgn_item sgi then
                                            S.Return ()
                                        else
                                            S.Continue (sgi, ()),
                  sgn = fn x => fn () =>
                                   if sgn x then
                                       S.Return ()
                                   else
                                       S.Continue (x, ()),
                  str = fn x => fn () =>
                                   if str x then
                                       S.Return ()
                                   else
                                       S.Continue (x, ()),
                  decl = fn x => fn () =>
                                    if decl x then
                                        S.Return ()
                                    else
                                        S.Continue (x, ())} k () of
        S.Return _ => true
      | S.Continue _ => false

fun search {kind, con, exp, sgn_item, sgn, str, decl} k =
    case mapfold {kind = fn x => fn () =>
                                    case kind x of
                                        NONE => S.Continue (x, ())
                                      | SOME v => S.Return v,

                  con = fn x => fn () =>
                                   case con x of
                                       NONE => S.Continue (x, ())
                                     | SOME v => S.Return v,

                  exp = fn x => fn () =>
                                   case exp x of
                                       NONE => S.Continue (x, ())
                                     | SOME v => S.Return v,

                  sgn_item = fn x => fn () =>
                                        case sgn_item x of
                                            NONE => S.Continue (x, ())
                                          | SOME v => S.Return v,

                  sgn = fn x => fn () =>
                                   case sgn x of
                                       NONE => S.Continue (x, ())
                                     | SOME v => S.Return v,

                  str = fn x => fn () =>
                                   case str x of
                                       NONE => S.Continue (x, ())
                                     | SOME v => S.Return v,

                  decl = fn x => fn () =>
                                    case decl x of
                                        NONE => S.Continue (x, ())
                                      | SOME v => S.Return v

                 } k () of
        S.Return x => SOME x
      | S.Continue _ => NONE

fun foldMapB {kind, con, exp, sgn_item, sgn, str, decl, bind} ctx st d =
    case mapfoldB {kind = fn x => fn st => S.Continue (kind (x, st)),
                   con = fn ctx => fn x => fn st => S.Continue (con (ctx, x, st)),
                   exp = fn ctx => fn x => fn st => S.Continue (exp (ctx, x, st)),
                   sgn_item = fn ctx => fn x => fn st => S.Continue (sgn_item (ctx, x, st)),
                   sgn = fn ctx => fn x => fn st => S.Continue (sgn (ctx, x, st)),
                   str = fn ctx => fn x => fn st => S.Continue (str (ctx, x, st)),
                   decl = fn ctx => fn x => fn st => S.Continue (decl (ctx, x, st)),
                   bind = bind} ctx d st of
        S.Continue x => x
      | S.Return _ => raise Fail "ElabUtil.Decl.foldMapB: Impossible"

end

structure File = struct

fun maxName ds = foldl (fn (d, count) => Int.max (maxNameDecl d, count)) 0 ds

and maxNameDecl (d, _) =
    case d of
        DCon (_, n, _, _) => n
      | DDatatype (_, n, _, ns) =>
                  foldl (fn ((_, n', _), m) => Int.max (n', m))
                        n ns
      | DDatatypeImp (_, n1, n2, _, _, _, ns) =>
        foldl (fn ((_, n', _), m) => Int.max (n', m))
              (Int.max (n1, n2)) ns
      | DVal (_, n, _, _) => n
      | DValRec vis => foldl (fn ((_, n, _, _), count) => Int.max (n, count)) 0 vis
      | DStr (_, n, sgn, str) => Int.max (n, Int.max (maxNameSgn sgn, maxNameStr str))
      | DSgn (_, n, sgn) => Int.max (n, maxNameSgn sgn)
      | DFfiStr (_, n, sgn) => Int.max (n, maxNameSgn sgn)
      | DConstraint _ => 0
      | DClass (_, n, _) => n
      | DExport _ => 0
      | DTable (n1, _, n2, _) => Int.max (n1, n2)
      | DSequence (n1, _, n2) => Int.max (n1, n2)
      | DDatabase _ => 0
      | DCookie (n1, _, n2, _) => Int.max (n1, n2)

and maxNameStr (str, _) =
    case str of
        StrConst ds => maxName ds
      | StrVar n => n
      | StrProj (str, _) => maxNameStr str
      | StrFun (_, n, dom, ran, str) => foldl Int.max n [maxNameSgn dom, maxNameSgn ran, maxNameStr str]
      | StrApp (str1, str2) => Int.max (maxNameStr str1, maxNameStr str2)
      | StrError => 0

and maxNameSgn (sgn, _) =
    case sgn of
        SgnConst sgis => foldl (fn (sgi, count) => Int.max (maxNameSgi sgi, count)) 0 sgis
      | SgnVar n => n
      | SgnFun (_, n, dom, ran) => Int.max (n, Int.max (maxNameSgn dom, maxNameSgn ran))
      | SgnWhere (sgn, _, _) => maxNameSgn sgn
      | SgnProj (n, _, _) => n
      | SgnError => 0

and maxNameSgi (sgi, _) =
    case sgi of
        SgiConAbs (_, n, _) => n
      | SgiCon (_, n, _, _) => n
      | SgiDatatype (_, n, _, ns) =>
        foldl (fn ((_, n', _), m) => Int.max (n', m))
              n ns
      | SgiDatatypeImp (_, n1, n2, _, _, _, ns) =>
        foldl (fn ((_, n', _), m) => Int.max (n', m))
              (Int.max (n1, n2)) ns
      | SgiVal (_, n, _) => n
      | SgiStr (_, n, sgn) => Int.max (n, maxNameSgn sgn)
      | SgiSgn (_, n, sgn) => Int.max (n, maxNameSgn sgn)
      | SgiConstraint _ => 0
      | SgiClassAbs (_, n) => n
      | SgiClass (_, n, _) => n
              
end

end
