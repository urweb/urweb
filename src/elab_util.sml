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

fun mapfold (f : (Elab.kind', 'state, 'abort) S.mapfold_arg) : (Elab.kind, 'state, 'abort) S.mapfolder =
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
    case mapfold (fn (k, ()) =>
                     if f k then
                         S.Return ()
                     else
                         S.Continue (k, ())) k () of
        S.Return _ => true
      | S.Continue _ => false

end

structure E = ElabEnv

fun declBinds env (d, _) =
    case d of
        DCon (x, k, _) => #1 (E.pushCNamed env x k)

end
