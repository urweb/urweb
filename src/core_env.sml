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

structure CoreEnv :> CORE_ENV = struct

open Core

structure U = CoreUtil

structure IM = IntBinaryMap


(* AST utility functions *)

val liftConInCon =
    U.Con.mapB {kind = fn k => k,
                con = fn bound => fn c =>
                                     case c of
                                         CRel xn =>
                                         if xn < bound then
                                             c
                                         else
                                             CRel (xn + 1)
                                       | _ => c,
                bind = fn (bound, U.Con.Rel _) => bound + 1
                        | (bound, _) => bound}

val lift = liftConInCon 0


(* Back to environments *)

exception UnboundRel of int
exception UnboundNamed of int

type env = {
     relC : (string * kind) list,
     namedC : (string * kind * con option) IM.map,

     relE : (string * con) list,
     namedE : (string * con * exp option * string) IM.map
}

val empty = {
    relC = [],
    namedC = IM.empty,

    relE = [],
    namedE = IM.empty
}

fun pushCRel (env : env) x k =
    {relC = (x, k) :: #relC env,
     namedC = IM.map (fn (x, k, co) => (x, k, Option.map lift co)) (#namedC env),

     relE = map (fn (x, c) => (x, lift c)) (#relE env),
     namedE = IM.map (fn (x, c, eo, s) => (x, lift c, eo, s)) (#namedE env)}

fun lookupCRel (env : env) n =
    (List.nth (#relC env, n))
    handle Subscript => raise UnboundRel n

fun pushCNamed (env : env) x n k co =
    {relC = #relC env,
     namedC = IM.insert (#namedC env, n, (x, k, co)),

     relE = #relE env,
     namedE = #namedE env}

fun lookupCNamed (env : env) n =
    case IM.find (#namedC env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushERel (env : env) x t =
    {relC = #relC env,
     namedC = #namedC env,

     relE = (x, t) :: #relE env,
     namedE = #namedE env}

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun pushENamed (env : env) x n t eo s =
    {relC = #relC env,
     namedC = #namedC env,

     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t, eo, s))}

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun declBinds env (d, _) =
    case d of
        DCon (x, n, k, c) => pushCNamed env x n k (SOME c)
      | DVal (x, n, t, e, s) => pushENamed env x n t (SOME e) s
      | DExport _ => env

end
