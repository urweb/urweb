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

structure FlatUtil :> FLAT_UTIL = struct

open Flat

structure S = Search

structure Typ = struct

fun join (o1, o2) =
    case o1 of
        EQUAL => o2 ()
      | v => v

fun joinL f (os1, os2) =
    case (os1, os2) of
        (nil, nil) => EQUAL
      | (nil, _) => LESS
      | (h1 :: t1, h2 :: t2) =>
        join (f (h1, h2), fn () => joinL f (t1, t2))
      | (_ :: _, nil) => GREATER

fun compare ((t1, _), (t2, _)) =
    case (t1, t2) of
        (TTop, TTop) => EQUAL
      | (TFun (d1, r1), TFun (d2, r2)) =>
        join (compare (d1, d2), fn () => compare (r1, r2))
      | (TCode (d1, r1), TCode (d2, r2)) =>
        join (compare (d1, d2), fn () => compare (r1, r2))
      | (TRecord xts1, TRecord xts2) =>
        let
            val xts1 = sortFields xts1
            val xts2 = sortFields xts2
        in
            joinL compareFields (xts1, xts2)
        end
      | (TNamed n1, TNamed n2) => Int.compare (n1, n2)
      | (TFfi (m1, x1), TFfi (m2, x2)) => join (String.compare (m1, m2), fn () => String.compare (x1, x2))

      | (TTop, _) => LESS
      | (_, TTop) => GREATER

      | (TFun _, _) => LESS
      | (_, TFun _) => GREATER

      | (TCode _, _) => LESS
      | (_, TCode _) => GREATER

      | (TRecord _, _) => LESS
      | (_, TRecord _) => GREATER

      | (TNamed _, _) => LESS
      | (_, TNamed _) => GREATER

and compareFields ((x1, t1), (x2, t2)) =
    join (String.compare (x1, x2),
          fn () => compare (t1, t2))

and sortFields xts = ListMergeSort.sort (fn (x, y) => compareFields (x, y) = GREATER) xts

fun mapfold fc =
    let
        fun mft c acc =
            S.bindP (mft' c acc, fc)

        and mft' (cAll as (c, loc)) =
            case c of
                TTop => S.return2 cAll
              | TFun (t1, t2) =>
                S.bind2 (mft t1,
                      fn t1' =>
                         S.map2 (mft t2,
                              fn t2' =>
                                 (TFun (t1', t2'), loc)))
              | TCode (t1, t2) =>
                S.bind2 (mft t1,
                      fn t1' =>
                         S.map2 (mft t2,
                              fn t2' =>
                                 (TCode (t1', t2'), loc)))
              | TRecord xts =>
                S.map2 (ListUtil.mapfold (fn (x, t) =>
                                             S.map2 (mft t,
                                                  fn t' =>
                                                     (x, t')))
                                         xts,
                     fn xts' => (TRecord xts', loc))
              | TNamed _ => S.return2 cAll
              | TFfi _ => S.return2 cAll
    in
        mft
    end

fun map typ c =
    case mapfold (fn c => fn () => S.Continue (typ c, ())) c () of
        S.Return () => raise Fail "Flat_util.Typ.map"
      | S.Continue (c, ()) => c

fun fold typ s c =
    case mapfold (fn c => fn s => S.Continue (c, typ (c, s))) c s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "FlatUtil.Typ.fold: Impossible"

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
         NamedT of string * int * typ option
       | RelE of string * typ
       | NamedE of string * int * typ * exp option

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
              | EFfi _ => S.return2 eAll
              | EFfiApp (m, x, es) =>
                S.map2 (ListUtil.mapfold (fn e => mfe ctx e) es,
                     fn es' =>
                        (EFfiApp (m, x, es'), loc))
              | ECode _ => S.return2 eAll
              | EApp (e1, e2) =>
                S.bind2 (mfe ctx e1,
                     fn e1' =>
                        S.map2 (mfe ctx e2,
                             fn e2' =>
                                (EApp (e1', e2'), loc)))

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

              | ELet (xes, e) =>
                S.bind2 (ListUtil.mapfold (fn (x, t, e) =>
                                              S.bind2 (mft t,
                                                       fn t' =>
                                                          S.map2 (mfe ctx e,
                                                               fn e' =>
                                                                  (x, t', e'))))
                                          xes,
                      fn xes' =>
                         S.map2 (mfe ctx e,
                                 fn e' =>
                                    (ELet (xes', e'), loc)))
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
      | S.Return _ => raise Fail "FlatUtil.Exp.mapB: Impossible"

fun map {typ, exp} e =
    case mapfold {typ = fn c => fn () => S.Continue (typ c, ()),
                  exp = fn e => fn () => S.Continue (exp e, ())} e () of
        S.Return () => raise Fail "Flat_util.Exp.map"
      | S.Continue (e, ()) => e

fun fold {typ, exp} s e =
    case mapfold {typ = fn c => fn s => S.Continue (c, typ (c, s)),
                  exp = fn e => fn s => S.Continue (e, exp (e, s))} e s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "FlatUtil.Exp.fold: Impossible"

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
                DVal (x, n, t, e) =>
                S.bind2 (mft t,
                      fn t' =>
                         S.map2 (mfe ctx e,
                              fn e' =>
                                 (DVal (x, n, t', e'), loc)))
              | DFun (n, x, dom, ran, e) =>
                S.bind2 (mft dom,
                      fn dom' =>
                         S.bind2 (mft ran,
                               fn ran' =>
                                  S.map2 (mfe ctx e,
                                       fn e' =>
                                          (DFun (n, x, dom', ran', e'), loc))))
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
      | S.Return _ => raise Fail "FlatUtil.Decl.fold: Impossible"

end

structure File = struct

datatype binder =
         NamedT of string * int * typ option
       | RelE of string * typ
       | NamedE of string * int * typ * exp option
       | F of int * string * Flat.typ * Flat.typ * Flat.exp

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
                                val b =
                                    case #1 d' of
                                        DVal (x, n, t, e) => NamedE (x, n, t, SOME e)
                                      | DFun v => F v
                                val ctx' = bind (ctx, b)
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
      | S.Return _ => raise Fail "FlatUtil.File.mapB: Impossible"

fun fold {typ, exp, decl} s d =
    case mapfold {typ = fn c => fn s => S.Continue (c, typ (c, s)),
                  exp = fn e => fn s => S.Continue (e, exp (e, s)),
                  decl = fn d => fn s => S.Continue (d, decl (d, s))} d s of
        S.Continue (_, s) => s
      | S.Return _ => raise Fail "FlatUtil.File.fold: Impossible"

end

end
