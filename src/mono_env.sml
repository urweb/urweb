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

structure MonoEnv :> MONO_ENV = struct

open Mono

structure IM = IntBinaryMap


exception UnboundRel of int
exception UnboundNamed of int

type env = {
     namedT : (string * typ option) IM.map,

     relE : (string * typ) list,
     namedE : (string * typ * exp option * string) IM.map
}

val empty = {
    namedT = IM.empty,

    relE = [],
    namedE = IM.empty
}

fun pushTNamed (env : env) x n co =
    {namedT = IM.insert (#namedT env, n, (x, co)),

     relE = #relE env,
     namedE = #namedE env}

fun lookupTNamed (env : env) n =
    case IM.find (#namedT env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushERel (env : env) x t =
    {namedT = #namedT env,

     relE = (x, t) :: #relE env,
     namedE = #namedE env}

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun pushENamed (env : env) x n t eo s =
    {namedT = #namedT env,

     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t, eo, s))}

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun declBinds env (d, _) =
    case d of
        DVal (x, n, t, e, s) => pushENamed env x n t (SOME e) s
      | DExport _ => env

end
