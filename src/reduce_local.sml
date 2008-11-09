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

(* Simplify a Core program algebraically, without unfolding definitions *)

structure ReduceLocal :> REDUCE_LOCAL = struct

open Core

structure E = CoreEnv
structure U = CoreUtil

val subExpInExp = E.subExpInExp

fun default x = x

fun exp (e : exp') =
    let
        (*val () = Print.prefaces "exp" [("e", CorePrint.p_exp env (e, ErrorMsg.dummySpan))]*)

        val r = case e of
                    EApp ((EAbs (x, t, _, e1), loc), e2) =>
                    ((*Print.prefaces "Substitute" [("x", Print.PD.string x),
                                                  ("t", CorePrint.p_con CoreEnv.empty t)];*)
                     #1 (reduceExp (subExpInExp (0, e2) e1)))

                  | EField ((ERecord xes, _), (CName x, _), _) =>
                    (case List.find (fn ((CName x', _), _, _) => x' = x
                                      | _ => false) xes of
                         SOME (_, (e, _), _) => e
                       | NONE => e)

                  | _ => e
    in
        (*Print.prefaces "exp'" [("e", CorePrint.p_exp env (e, ErrorMsg.dummySpan)),
                               ("r", CorePrint.p_exp env (r, ErrorMsg.dummySpan))];*)

        r
    end

and reduceExp e = U.Exp.map {kind = default, con = default, exp = exp} e

val reduce = U.File.map {kind = default, con = default, exp = exp, decl = default}

end
