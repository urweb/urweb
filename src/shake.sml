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

(* Remove unused definitions from a file *)

structure Shake :> SHAKE = struct

open Core

structure U = CoreUtil

structure IS = IntBinarySet
structure IM = IntBinaryMap

type free = {
     con : IS.set,
     exp : IS.set
}

fun shake file =
    case List.foldl (fn ((DVal ("main", n, _, e), _), _) => SOME (n, e)
                      | (_, s) => s) NONE file of
        NONE => []
      | SOME (main, body) =>
        let
            val (cdef, edef) = foldl (fn ((DCon (_, n, _, c), _), (cdef, edef)) => (IM.insert (cdef, n, c), edef)
                                       | ((DVal (_, n, t, e), _), (cdef, edef)) => (cdef, IM.insert (edef, n, (t, e))))
                               (IM.empty, IM.empty) file

            fun kind (_, s) = s

            fun con (c, s) =
                case c of
                    CNamed n =>
                    if IS.member (#con s, n) then
                        s
                    else
                        let
                            val s' = {con = IS.add (#con s, n),
                                      exp = #exp s}
                        in
                            case IM.find (cdef, n) of
                                NONE => s'
                              | SOME c => shakeCon s' c
                        end
                  | _ => s

            and shakeCon s = U.Con.fold {kind = kind, con = con} s

            fun exp (e, s) =
                case e of
                    ENamed n =>
                    if IS.member (#exp s, n) then
                        s
                    else
                        let
                            val s' = {exp = IS.add (#exp s, n),
                                      con = #con s}
                        in
                            case IM.find (edef, n) of
                                NONE => s'
                              | SOME (t, e) => shakeExp (shakeCon s' t) e
                        end
                  | _ => s

            and shakeExp s = U.Exp.fold {kind = kind, con = con, exp = exp} s

            val s = {con = IS.empty,
                     exp = IS.singleton main}
                    
            val s = U.Exp.fold {kind = kind, con = con, exp = exp} s body
        in
            List.filter (fn (DCon (_, n, _, _), _) => IS.member (#con s, n)
                          | (DVal (_, n, _, _), _) => IS.member (#exp s, n)) file
        end

end
