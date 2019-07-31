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

structure Search :> SEARCH = struct

datatype ('state, 'abort) result =
         Return of 'abort
       | Continue of 'state

type ('data, 'state, 'abort) mapfold_arg =
     'data * 'state -> ('data * 'state, 'abort) result

type ('data, 'state, 'abort) mapfolder =
     'data -> 'state -> ('data * 'state, 'abort) result

type ('context, 'data, 'state, 'abort) mapfolderB =
     'context -> 'data -> 'state -> ('data * 'state, 'abort) result

fun return2 v acc = Continue (v, acc)

fun map (r, f) =
    case r of
        Continue acc => Continue (f acc)
      | Return x => Return x

fun map2 (r, f) acc =
    case r acc of
        Continue (x, acc) => Continue (f x, acc)
      | Return x => Return x

fun bind (r, f) =
    case r of
        Continue acc => f acc
      | Return x => Return x

fun bind2 (r, f) acc =
    case r acc of
        Continue (x, acc) => f x acc
      | Return x => Return x

fun bindP (r, f) =
    case r of
        Continue ((x, pos), acc) =>
        map (f x acc,
          fn (x', acc') =>
             ((x', pos), acc'))
      | Return x => Return x

fun bindPWithPos (r, f) =
    case r of
        Continue ((x, pos), acc) =>
        map (f (x, pos) acc,
             fn (x', acc') =>
                ((x', pos), acc'))
      | Return x => Return x

end
