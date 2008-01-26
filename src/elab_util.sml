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

              | KError => S.return2 kAll

              | KUnif (_, ref (SOME k)) => mfk' k
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

fun mapfold {kind = fk, con = fc} =
    let
        val mfk = Kind.mapfold fk

        fun mfc c acc =
            S.bindP (mfc' c acc, fc)

        and mfc' (cAll as (c, loc)) =
            case c of
                TFun (c1, c2) =>
                S.bind2 (mfc c1,
                      fn c1' =>
                         S.map2 (mfc c2,
                              fn c2' =>
                                 (TFun (c1', c2'), loc)))
              | TCFun (e, x, k, c) =>
                S.bind2 (mfk k,
                      fn k' =>
                         S.map2 (mfc c,
                              fn c' =>
                                 (TCFun (e, x, k', c'), loc)))
              | TRecord c =>
                S.map2 (mfc c,
                        fn c' =>
                           (TRecord c', loc))

              | CRel _ => S.return2 cAll
              | CNamed _ => S.return2 cAll
              | CApp (c1, c2) =>
                S.bind2 (mfc c1,
                      fn c1' =>
                         S.map2 (mfc c2,
                              fn c2' =>
                                 (CApp (c1', c2'), loc)))
              | CAbs (x, k, c) =>
                S.bind2 (mfk k,
                      fn k' =>
                         S.map2 (mfc c,
                              fn c' =>
                                 (CAbs (x, k', c'), loc)))

              | CName _ => S.return2 cAll

              | CRecord (k, xcs) =>
                S.bind2 (mfk k,
                      fn k' =>
                         S.map2 (ListUtil.mapfold (fn (x, c) =>
                                                      S.bind2 (mfc x,
                                                            fn x' =>
                                                               S.map2 (mfc c,
                                                                    fn c' =>
                                                                       (x', c'))))
                                 xcs,
                              fn xcs' =>
                                 (CRecord (k', xcs'), loc)))
              | CConcat (c1, c2) =>
                S.bind2 (mfc c1,
                      fn c1' =>
                         S.map2 (mfc c2,
                              fn c2' =>
                                 (CConcat (c1', c2'), loc)))

              | CError => S.return2 cAll
              | CUnif (_, _, ref (SOME c)) => mfc' c
              | CUnif _ => S.return2 cAll
    in
        mfc
    end

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

structure E = ElabEnv

fun declBinds env (d, _) =
    case d of
        DCon (x, n, k, _) => E.pushCNamedAs env x n k
      | DVal (x, n, t, _) => E.pushENamedAs env x n t

end
