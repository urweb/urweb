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

structure PathCheck :> PATH_CHECK = struct

open Mono

structure E = ErrorMsg

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

fun checkDecl ((d, loc), (funcs, rels)) =
    let
        fun doRel s =
            (if SS.member (rels, s) then
                 E.errorAt loc ("Duplicate table/sequence path " ^ s)
             else
                 ();
             (funcs, SS.add (rels, s)))
    in
        case d of
            DExport (_, s, _, _, _) =>
            (if SS.member (funcs, s) then
                 E.errorAt loc ("Duplicate function path " ^ s)
             else
                 ();
             (SS.add (funcs, s), rels))
            
          | DTable (s, _) => doRel s
          | DSequence s => doRel s

          | _ => (funcs, rels)
    end

fun check ds = ignore (foldl checkDecl (SS.empty, SS.empty) ds)

end
