(* Copyright (c) 2011, Adam Chlipala
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

structure SideCheck :> SIDE_CHECK = struct

open Mono

structure E = ErrorMsg

structure FS = BinarySetFn(struct
                           type ord_key = string * string
                           fun compare ((x1, y1), (x2, y2)) = Order.join (String.compare (x1, x2),
                                                                          fn () => String.compare (y1, y2))
                           end)

fun check ds =
    let
        val fs = MonoUtil.File.fold {typ = fn (_, fs) => fs,
                                     exp = fn (e, fs) =>
                                              case e of
                                                  EFfi k => FS.add (fs, k)
                                                | EFfiApp (k1, k2, _) => FS.add (fs, (k1, k2))
                                                | _ => fs,
                                     decl = fn (_, fs) => fs}
                                    FS.empty ds
    in
        FS.app (fn k as (k1, k2) =>
                   if Settings.isClientOnly k then
                       E.error ("Server-side code uses client-side-only identifier \"" ^ k1 ^ "." ^ k2 ^ "\"")
                   else
                       ()) fs;
        ds
    end

end
