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

structure MonoUtil :> MONO_UTIL = struct

open Mono

structure S = Search

val dummyt = (TRecord [], ErrorMsg.dummySpan)

structure Typ = struct

open Order

fun compare ((t1, _), (t2, _)) =
    case (t1, t2) of
        (TFun (d1, r1), TFun (d2, r2)) =>
        join (compare (d1, d2), fn () => compare (r1, r2))
      | (TRecord xts1, TRecord xts2) =>
        let
            val xts1 = sortFields xts1
            val xts2 = sortFields xts2
        in
            joinL compareFields (xts1, xts2)
        end
      | (TDatatype (n1, _), TDatatype (n2, _)) => Int.compare (n1, n2)
      | (TFfi (m1, x1), TFfi (m2, x2)) => join (String.compare (m1, m2), fn () => String.compare (x1, x2))
      | (TOption t1, TOption t2) => compare (t1, t2)
      | (TList t1, TList t2) => compare (t1, t2)
      | (TSource, TSource) => EQUAL
      | (TSignal t1, TSignal t2) => compare (t1, t2)

      | (TFun _, _) => LESS
      | (_, TFun _) => GREATER

      | (TRecord _, _) => LESS
      | (_, TRecord _) => GREATER

      | (TDatatype _, _) => LESS
      | (_, TDatatype _) => GREATER

      | (TFfi _, _) => LESS
      | (_, TFfi _) => GREATER

      | (TOption _, _) => LESS
      | (_, TOption _) => GREATER

      | (TList _, _) => LESS
      | (_, TList _) => GREATER

      | (TSource, _) => LESS
      | (_, TSource) => GREATER

and compareFields ((x1, t1), (x2, t2)) =
    join (String.compare (x1, x2),
          fn () => compare (t1, t2))

and sortFields xts = ListMergeSort.sort (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) xts

fun mapfold fc =
    let
        fun mft c acc =
            S.bindP (mft' c acc, fc)

        and mft' (cAll as (c, loc)) =
            case c of
                TFun (t1, t2) =>
                S.bind2 (mft t1,
                      fn t1' =>
                         S.map2 (mft t2,
                              fn t2' =>
                                 (TFun (t1', t2'), loc)))
              | TRecord xts =>
                S.map2 (ListUtil.mapfold (fn (x, t) =>
                                             S.map2 (mft t,
                                                  fn t' =>
                                                     (x, t')))
                                         xts,
                     fn xts' => (TRecord xts', loc))
              | TDatatype _ => S.return2 cAll
              | TFfi _ => S.return2 cAll
              | TOption t =>
                S.map2 (mft t,
                        fn t' =>
                           (TOption t, loc))
              | TList t =>
                S.map2 (mft t,
                        fn t' =>
                           (TList t, loc))
              | TSource => S.return2 cAll
              | TSignal t =>
                S.map2 (mft t,
                        fn t' =>
                           (TSignal t, loc))
    in
        mft
    end

fun map typ c =
    case mapfold (fn c => fn () => S.Continue (typ c, ())) c () of
        S.Return () => raise Fail "Mono_util.Typ.map"
      | S.Continue (c, ()) => c

fun fold typ s c =
    case mapfold (fn c => fn s => S.Continue (c, typ (c, s))) c s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "MonoUtil.Typ.fold: Impossible"

fun exists typ k =
    case mapfold (fn c => fn () =>
                             if typ c then
                                 S.Return ()
                             else
                                 S.Continue (c, ())) k () of
        S.Return _ => true
      | S.Continue _ => false

end

structure Exp = struct

datatype binder =
         Datatype of string * int * (string * int * typ option) list
       | RelE of string * typ
       | NamedE of string * int * typ * exp option * string

fun mapfoldB {typ = fc, exp = fe, bind} =
    let
        val mft = Typ.mapfold fc

        fun mfe ctx e acc =
            S.bindP (mfe' ctx e acc, fe ctx)

        and mfe' ctx (eAll as (e, loc)) =
            case e of
                EPrim _ => S.return2 eAll
              | ERel _ => S.return2 eAll
              | ENamed _ => S.return2 eAll
              | ECon (_, _, NONE) => S.return2 eAll
              | ECon (dk, n, SOME e) =>
                S.map2 (mfe ctx e,
                        fn e' =>
                           (ECon (dk, n, SOME e'), loc))
              | ENone t =>
                S.map2 (mft t,
                        fn t' =>
                           (ENone t', loc))
              | ESome (t, e) =>
                S.bind2 (mft t,
                         fn t' =>
                            S.map2 (mfe ctx e,
                                 fn e' =>
                                    (ESome (t', e'), loc)))
              | EFfi _ => S.return2 eAll
              | EFfiApp (m, x, es) =>
                S.map2 (ListUtil.mapfold (fn e => mfe ctx e) es,
                     fn es' =>
                        (EFfiApp (m, x, es'), loc))
              | EApp (e1, e2) =>
                S.bind2 (mfe ctx e1,
                      fn e1' =>
                         S.map2 (mfe ctx e2,
                              fn e2' =>
                                 (EApp (e1', e2'), loc)))
              | EAbs (x, dom, ran, e) =>
                S.bind2 (mft dom,
                      fn dom' =>
                         S.bind2 (mft ran,
                               fn ran' =>
                                  S.map2 (mfe (bind (ctx, RelE (x, dom'))) e,
                                       fn e' =>
                                          (EAbs (x, dom', ran', e'), loc))))

              | EUnop (s, e) =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (EUnop (s, e'), loc))
              | EBinop (bi, s, e1, e2) =>
                S.bind2 (mfe ctx e1,
                      fn e1' =>
                         S.map2 (mfe ctx e2,
                              fn e2' =>
                                 (EBinop (bi, s, e1', e2'), loc)))

              | ERecord xes =>
                S.map2 (ListUtil.mapfold (fn (x, e, t) =>
                                             S.bind2 (mfe ctx e,
                                                  fn e' =>
                                                     S.map2 (mft t,
                                                             fn t' =>
                                                                (x, e', t'))))
                                         xes,
                     fn xes' =>
                        (ERecord xes', loc))
              | EField (e, x) =>
                S.map2 (mfe ctx e,
                      fn e' =>
                         (EField (e', x), loc))

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
                                                                    | PCon (_, _, NONE) => ctx
                                                                    | PCon (_, _, SOME p) => pb (p, ctx)
                                                                    | PRecord xps => foldl (fn ((_, p, _), ctx) =>
                                                                                               pb (p, ctx)) ctx xps
                                                                    | PNone _ => ctx
                                                                    | PSome (_, p) => pb (p, ctx)
                                                          in
                                                              S.map2 (mfe (pb (p, ctx)) e,
                                                                   fn e' => (p, e'))
                                                          end) pes,
                                    fn pes' =>
                                       S.bind2 (mft disc,
                                                fn disc' =>
                                                   S.map2 (mft result,
                                                        fn result' =>
                                                           (ECase (e', pes', {disc = disc', result = result'}), loc)))))

              | EError (e, t) =>
                S.bind2 (mfe ctx e,
                         fn e' =>
                            S.map2 (mft t,
                                    fn t' =>
                                       (EError (e', t'), loc)))
              | EReturnBlob {blob, mimeType, t} =>
                S.bind2 (mfe ctx blob,
                         fn blob' =>
                            S.bind2 (mfe ctx mimeType,
                                  fn mimeType' =>
                                     S.map2 (mft t,
                                          fn t' =>
                                             (EReturnBlob {blob = blob', mimeType = mimeType', t = t'}, loc))))
              | ERedirect (e, t) =>
                S.bind2 (mfe ctx e,
                         fn e' =>
                            S.map2 (mft t,
                                    fn t' =>
                                       (ERedirect (e', t'), loc)))
                            
              | EStrcat (e1, e2) =>
                S.bind2 (mfe ctx e1,
                      fn e1' =>
                         S.map2 (mfe ctx e2,
                              fn e2' =>
                                 (EStrcat (e1', e2'), loc)))

              | EWrite e =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (EWrite e', loc))

              | ESeq (e1, e2) =>
                S.bind2 (mfe ctx e1,
                      fn e1' =>
                         S.map2 (mfe ctx e2,
                              fn e2' =>
                                 (ESeq (e1', e2'), loc)))
              | ELet (x, t, e1, e2) =>
                S.bind2 (mft t,
                         fn t' =>
                            S.bind2 (mfe ctx e1,
                                  fn e1' =>
                                     S.map2 (mfe (bind (ctx, RelE (x, t'))) e2,
                                          fn e2' =>
                                             (ELet (x, t', e1', e2'), loc))))

              | EClosure (n, es) =>
                S.map2 (ListUtil.mapfold (mfe ctx) es,
                     fn es' =>
                        (EClosure (n, es'), loc))

              | EQuery {exps, tables, state, query, body, initial} =>
                S.bind2 (ListUtil.mapfold (fn (x, t) =>
                                              S.map2 (mft t,
                                                      fn t' => (x, t'))) exps,
                         fn exps' =>
                            S.bind2 (ListUtil.mapfold (fn (x, xts) =>
                                                          S.map2 (ListUtil.mapfold
                                                                       (fn (x, t) =>
                                                                           S.map2 (mft t,
                                                                                fn t' => (x, t'))) xts,
                                                                   fn xts' => (x, xts'))) tables,
                                  fn tables' =>
                                     S.bind2 (mft state,
                                              fn state' =>
                                                 S.bind2 (mfe ctx query,
                                                          fn query' =>
                                                             S.bind2 (mfe (bind (bind (ctx, RelE ("r", dummyt)),
                                                                                 RelE ("acc", dummyt)))
                                                                          body,
                                                                   fn body' =>
                                                                      S.map2 (mfe ctx initial,
                                                                           fn initial' =>
                                                                              (EQuery {exps = exps',
                                                                                       tables = tables',
                                                                                       state = state',
                                                                                       query = query',
                                                                                       body = body',
                                                                                       initial = initial'},
                                                                               loc)))))))

              | EDml (e, fm) =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (EDml (e', fm), loc))
              | ENextval e =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (ENextval e', loc))
              | ESetval (e1, e2) =>
                S.bind2 (mfe ctx e1,
                     fn e1' =>
                        S.map2 (mfe ctx e2,
                             fn e2' =>
                                (ESetval (e1', e2'), loc)))
              | EUnurlify (e, t, b) =>
                S.bind2 (mfe ctx e,
                     fn e' =>
                        S.map2 (mft t,
                                fn t' =>
                                   (EUnurlify (e', t', b), loc)))
              | EJavaScript (m, e) =>
                S.bind2 (mfmode ctx m,
                         fn m' =>
                            S.map2 (mfe ctx e,
                                 fn e' =>
                                    (EJavaScript (m', e'), loc)))

              | ESignalReturn e =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (ESignalReturn e', loc))
              | ESignalBind (e1, e2) =>
                S.bind2 (mfe ctx e1,
                      fn e1' =>
                         S.map2 (mfe ctx e2,
                              fn e2' =>
                                 (ESignalBind (e1', e2'), loc)))
              | ESignalSource e =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (ESignalSource e', loc))

              | EServerCall (s, t, eff) =>
                S.bind2 (mfe ctx s,
                         fn s' =>
                            S.map2 (mft t,
                                  fn t' =>
                                     (EServerCall (s', t', eff), loc)))
              | ERecv (s, t) =>
                S.bind2 (mfe ctx s,
                      fn s' =>
                         S.map2 (mft t,
                              fn t' =>
                                 (ERecv (s', t'), loc)))
              | ESleep s =>
                S.map2 (mfe ctx s,
                      fn s' =>
                         (ESleep s', loc))

              | ESpawn s =>
                S.map2 (mfe ctx s,
                      fn s' =>
                         (ESpawn s', loc))

        and mfmode ctx mode =
            case mode of
                Attribute => S.return2 mode
              | Script => S.return2 mode
              | Source t =>
                S.map2 (mft t,
                     fn t' => Source t')
    in
        mfe
    end

fun mapfold {typ = fc, exp = fe} =
    mapfoldB {typ = fc,
              exp = fn () => fe,
              bind = fn ((), _) => ()} ()

fun mapB {typ, exp, bind} ctx e =
    case mapfoldB {typ = fn c => fn () => S.Continue (typ c, ()),
                   exp = fn ctx => fn e => fn () => S.Continue (exp ctx e, ()),
                   bind = bind} ctx e () of
        S.Continue (e, ()) => e
      | S.Return _ => raise Fail "MonoUtil.Exp.mapB: Impossible"

fun map {typ, exp} e =
    case mapfold {typ = fn c => fn () => S.Continue (typ c, ()),
                  exp = fn e => fn () => S.Continue (exp e, ())} e () of
        S.Return () => raise Fail "Mono_util.Exp.map"
      | S.Continue (e, ()) => e

fun fold {typ, exp} s e =
    case mapfold {typ = fn c => fn s => S.Continue (c, typ (c, s)),
                  exp = fn e => fn s => S.Continue (e, exp (e, s))} e s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "MonoUtil.Exp.fold: Impossible"

fun exists {typ, exp} k =
    case mapfold {typ = fn c => fn () =>
                                    if typ c then
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

fun existsB {typ, exp, bind} ctx e =
    case mapfoldB {typ = fn t => fn () =>
                                    if typ t then
                                        S.Return ()
                                    else
                                        S.Continue (t, ()),
                   exp = fn ctx => fn e => fn () =>
                                              if exp (ctx, e) then
                                                  S.Return ()
                                              else
                                                  S.Continue (e, ()),
                   bind = bind} ctx e () of
        S.Return _ => true
      | S.Continue _ => false

fun foldB {typ, exp, bind} ctx s e =
    case mapfoldB {typ = fn t => fn s => S.Continue (t, typ (t, s)),
                   exp = fn ctx => fn e => fn s => S.Continue (e, exp (ctx, e, s)),
                   bind = bind} ctx e s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "MonoUtil.Exp.foldB: Impossible"

end

structure Decl = struct

datatype binder = datatype Exp.binder

fun mapfoldB {typ = fc, exp = fe, decl = fd, bind} =
    let
        val mft = Typ.mapfold fc

        val mfe = Exp.mapfoldB {typ = fc, exp = fe, bind = bind}

        fun mfd ctx d acc =
            S.bindP (mfd' ctx d acc, fd ctx)

        and mfd' ctx (dAll as (d, loc)) =
            case d of
                DDatatype dts =>
                S.map2 (ListUtil.mapfold (fn (x, n, xncs) =>
                                             S.map2 (ListUtil.mapfold (fn (x, n, c) =>
                                                                          case c of
                                                                              NONE => S.return2 (x, n, c)
                                                                            | SOME c =>
                                                                              S.map2 (mft c,
                                                                                   fn c' => (x, n, SOME c'))) xncs,
                                                  fn xncs' => (x, n, xncs'))) dts,
                        fn dts' =>
                           (DDatatype dts', loc))
              | DVal vi =>
                S.map2 (mfvi ctx vi,
                     fn vi' =>
                        (DVal vi', loc))
              | DValRec vis =>
                let
                    val ctx' = foldl (fn ((x, n, t, _, s), ctx') => bind (ctx', NamedE (x, n, t, NONE, s))) ctx vis
                in
                    S.map2 (ListUtil.mapfold (mfvi ctx') vis,
                         fn vis' =>
                            (DValRec vis', loc))
                end
              | DExport (ek, s, n, ts, t, b) =>
                S.bind2 (ListUtil.mapfold mft ts,
                        fn ts' =>
                           S.map2 (mft t,
                                   fn t' =>
                                      (DExport (ek, s, n, ts', t', b), loc)))
              | DTable (s, xts, pe, ce) =>
                S.bind2 (mfe ctx pe,
                      fn pe' =>
                         S.map2 (mfe ctx ce,
                              fn ce' =>
                                 (DTable (s, xts, pe', ce'), loc)))
              | DSequence _ => S.return2 dAll
              | DView (s, xts, e) =>
                S.map2 (mfe ctx e,
                     fn e' =>
                        (DView (s, xts, e'), loc))
              | DDatabase _ => S.return2 dAll
              | DJavaScript _ => S.return2 dAll
              | DCookie _ => S.return2 dAll
              | DStyle _ => S.return2 dAll
              | DTask (e1, e2) =>
                S.bind2 (mfe ctx e1,
                     fn e1' =>
                        S.map2 (mfe ctx e2,
                             fn e2' =>
                                (DTask (e1', e2'), loc)))
              | DPolicy pol =>
                S.map2 (mfpol ctx pol,
                     fn p' =>
                        (DPolicy p', loc))
              | DOnError _ => S.return2 dAll

        and mfpol ctx pol =
            case pol of
                PolClient e =>
                S.map2 (mfe ctx e,
                        PolClient)
              | PolInsert e =>
                S.map2 (mfe ctx e,
                        PolInsert)
              | PolDelete e =>
                S.map2 (mfe ctx e,
                        PolDelete)
              | PolUpdate e =>
                S.map2 (mfe ctx e,
                        PolUpdate)
              | PolSequence e =>
                S.map2 (mfe ctx e,
                        PolSequence)

        and mfvi ctx (x, n, t, e, s) =
            S.bind2 (mft t,
                  fn t' =>
                     S.map2 (mfe ctx e,
                          fn e' =>
                             (x, n, t', e', s)))
    in
        mfd
    end    

fun mapfold {typ = fc, exp = fe, decl = fd} =
    mapfoldB {typ = fc,
              exp = fn () => fe,
              decl = fn () => fd,
              bind = fn ((), _) => ()} ()

fun fold {typ, exp, decl} s d =
    case mapfold {typ = fn c => fn s => S.Continue (c, typ (c, s)),
                  exp = fn e => fn s => S.Continue (e, exp (e, s)),
                  decl = fn d => fn s => S.Continue (d, decl (d, s))} d s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "MonoUtil.Decl.fold: Impossible"

fun map {typ, exp, decl} e =
    case mapfold {typ = fn c => fn () => S.Continue (typ c, ()),
                  exp = fn e => fn () => S.Continue (exp e, ()),
                  decl = fn d => fn () => S.Continue (decl d, ())} e () of
        S.Return () => raise Fail "MonoUtil.Decl.map: Impossible"
      | S.Continue (e, ()) => e

fun foldMapB {typ, exp, decl, bind} ctx s d =
    case mapfoldB {typ = fn c => fn s => S.Continue (typ (c, s)),
                   exp = fn ctx => fn e => fn s => S.Continue (exp (ctx, e, s)),
                   decl = fn ctx => fn d => fn s => S.Continue (decl (ctx, d, s)),
                   bind = bind} ctx d s of
        S.Continue v => v
      | S.Return _ => raise Fail "MonoUtil.Decl.foldMapB: Impossible"

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
                                        DDatatype dts =>
                                        foldl (fn ((x, n, xncs), ctx) =>
                                                  let
                                                      val ctx = bind (ctx, Datatype (x, n, xncs))
                                                      val t = (TDatatype (n, ref (ElabUtil.classifyDatatype xncs, xncs)),
                                                               #2 d')
                                                  in
                                                      foldl (fn ((x, n, to), ctx) =>
                                                                let
                                                                    val t = case to of
                                                                                NONE => t
                                                                              | SOME t' => (TFun (t', t), #2 d')
                                                                in
                                                                    bind (ctx, NamedE (x, n, t, NONE, ""))
                                                                end)
                                                            ctx xncs
                                                  end) ctx dts
                                      | DVal (x, n, t, e, s) => bind (ctx, NamedE (x, n, t, SOME e, s))
                                      | DValRec vis => foldl (fn ((x, n, t, e, s), ctx) =>
                                                                 bind (ctx, NamedE (x, n, t, NONE, s))) ctx vis
                                      | DExport _ => ctx
                                      | DTable _ => ctx
                                      | DSequence _ => ctx
                                      | DView _ => ctx
                                      | DDatabase _ => ctx
                                      | DJavaScript _ => ctx
                                      | DCookie _ => ctx
                                      | DStyle _ => ctx
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

fun mapfold {typ = fc, exp = fe, decl = fd} =
    mapfoldB {typ = fc,
              exp = fn () => fe,
              decl = fn () => fd,
              bind = fn ((), _) => ()} ()

fun mapB {typ, exp, decl, bind} ctx ds =
    case mapfoldB {typ = fn c => fn () => S.Continue (typ c, ()),
                   exp = fn ctx => fn e => fn () => S.Continue (exp ctx e, ()),
                   decl = fn ctx => fn d => fn () => S.Continue (decl ctx d, ()),
                   bind = bind} ctx ds () of
        S.Continue (ds, ()) => ds
      | S.Return _ => raise Fail "MonoUtil.File.mapB: Impossible"

fun map {typ, exp, decl} e =
    case mapfold {typ = fn c => fn () => S.Continue (typ c, ()),
                  exp = fn e => fn () => S.Continue (exp e, ()),
                  decl = fn d => fn () => S.Continue (decl d, ())} e () of
        S.Return () => raise Fail "MonoUtil.File.map: Impossible"
      | S.Continue (e, ()) => e

fun fold {typ, exp, decl} s d =
    case mapfold {typ = fn c => fn s => S.Continue (c, typ (c, s)),
                  exp = fn e => fn s => S.Continue (e, exp (e, s)),
                  decl = fn d => fn s => S.Continue (d, decl (d, s))} d s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "MonoUtil.File.fold: Impossible"

val maxName = foldl (fn ((d, _) : decl, count) =>
                        case d of
                            DDatatype dts =>
                            foldl (fn ((_, n, ns), count) =>
                                      foldl (fn ((_, n', _), m) => Int.max (n', m))
                                            (Int.max (n, count)) ns) count dts
                          | DVal (_, n, _, _, _) => Int.max (n, count)
                          | DValRec vis => foldl (fn ((_, n, _, _, _), count) => Int.max (n, count)) count vis
                          | DExport _ => count
                          | DTable _ => count
                          | DSequence _ => count
                          | DView _ => count
                          | DDatabase _ => count
                          | DJavaScript _ => count
                          | DCookie _ => count
                          | DStyle _ => count
                          | DTask _ => count
                          | DPolicy _ => count
                          | DOnError _ => count) 0

end

end
