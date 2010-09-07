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
     datatypes : (string * (string * int * typ option) list) IM.map,
     constructors : (string * typ option * int) IM.map,

     numRelE : int,
     relE : (string * typ) list,
     namedE : (string * typ) IM.map,

     structs : (string * typ) list IM.map
}

val empty : env = {
    datatypes = IM.empty,
    constructors = IM.empty,

    numRelE = 0,
    relE = [],
    namedE = IM.empty,

    structs = IM.insert (IM.empty, 0, [])
}

fun pushDatatype (env : env) x n xncs =
    {datatypes = IM.insert (#datatypes env, n, (x, xncs)),
     constructors = foldl (fn ((x, n', to), constructors) =>
                              IM.insert (constructors, n', (x, to, n)))
                          (#constructors env) xncs,

     numRelE = #numRelE env,
     relE = #relE env,
     namedE = #namedE env,

     structs = #structs env}

fun lookupDatatype (env : env) n =
    case IM.find (#datatypes env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupConstructor (env : env) n =
    case IM.find (#constructors env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushERel (env : env) x t =
    {datatypes = #datatypes env,
     constructors = #constructors env,

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
    {datatypes = #datatypes env,
     constructors = #constructors env,

     numRelE = #numRelE env,
     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t)),

     structs = #structs env}

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushStruct (env : env) n xts =
    {datatypes = #datatypes env,
     constructors = #constructors env,

     numRelE = #numRelE env,
     relE = #relE env,
     namedE = #namedE env,

     structs = IM.insert (#structs env, n, xts)}

fun lookupStruct (env : env) n =
    case IM.find (#structs env, n) of
        NONE => raise UnboundStruct n
      | SOME x => x

fun classifyDatatype xncs =
    if List.all (fn (_, _, NONE) => true | _ => false) xncs then
        Enum
    else
        Default

fun declBinds env (d, loc) =
    case d of
        DDatatype dts =>
        foldl (fn ((_, x, n, xncs), env) =>
                  let
                      val env = pushDatatype env x n xncs
                      val dt = (TDatatype (classifyDatatype xncs, n, ref xncs), loc)
                  in
                      foldl (fn ((x', n', NONE), env) => pushENamed env x' n' dt
                              | ((x', n', SOME t), env) => pushENamed env x' n' (TFun (t, dt), loc))
                            env xncs
                  end) env dts
      | DDatatypeForward (_, x, n) => pushDatatype env x n []
      | DStruct (n, xts) => pushStruct env n xts
      | DVal (x, n, t, _) => pushENamed env x n t
      | DFun (fx, n, args, ran, _) =>
        let
            val t = foldl (fn ((_, arg), t) => (TFun (arg, t), loc)) ran args
        in
            pushENamed env fx n t
        end
      | DFunRec vis =>
        foldl (fn ((fx, n, args, ran, _), env) =>
                  let
                      val t = foldl (fn ((_, arg), t) => (TFun (arg, t), loc)) ran args
                  in
                      pushENamed env fx n t
                  end) env vis
      | DTable _ => env
      | DSequence _ => env
      | DView _ => env
      | DDatabase _ => env
      | DPreparedStatements _ => env
      | DJavaScript _ => env
      | DCookie _ => env
      | DStyle _ => env
      | DTask _ => env
      | DOnError _ => env

end
