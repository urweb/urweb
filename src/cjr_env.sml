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

structure CjrEnv :> CJR_ENV = struct

open Cjr

structure IM = IntBinaryMap


exception UnboundRel of int
exception UnboundNamed of int
exception UnboundF of int
exception UnboundStruct of int

type env = {
     namedT : (string * typ option) IM.map,

     numRelE : int,
     relE : (string * typ) list,
     namedE : (string * typ) IM.map,

     structs : (string * typ) list IM.map
}

val empty = {
    namedT = IM.empty,

    numRelE = 0,
    relE = [],
    namedE = IM.empty,

    structs = IM.empty
}

fun pushTNamed (env : env) x n co =
    {namedT = IM.insert (#namedT env, n, (x, co)),

     numRelE = #numRelE env,
     relE = #relE env,
     namedE = #namedE env,

     structs = #structs env}

fun lookupTNamed (env : env) n =
    case IM.find (#namedT env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushERel (env : env) x t =
    {namedT = #namedT env,

     numRelE = #numRelE env + 1,
     relE = (x, t) :: #relE env,
     namedE = #namedE env,

     structs = #structs env}

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun countERels (env : env) = #numRelE env

fun listERels (env : env) = #relE env

fun pushENamed (env : env) x n t =
    {namedT = #namedT env,

     numRelE = #numRelE env,
     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t)),

     structs = #structs env}

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushStruct (env : env) n xts =
    {namedT = #namedT env,

     numRelE = #numRelE env,
     relE = #relE env,
     namedE = #namedE env,

     structs = IM.insert (#structs env, n, xts)}

fun lookupStruct (env : env) n =
    case IM.find (#structs env, n) of
        NONE => raise UnboundStruct n
      | SOME x => x

fun declBinds env (d, loc) =
    case d of
        DVal (x, n, t, _) => pushENamed env x n t
      | DFun (fx, n, _, dom, ran, _) => pushENamed env fx n (TFun (dom, ran), loc)
      | DStruct (n, xts) => pushStruct env n xts

end
