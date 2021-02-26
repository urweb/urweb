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
     datatypes : (string * (string * int * typ option) list) IM.map,
     constructors : (string * typ option * int) IM.map,

     relE : (string * typ * exp option) list,
     namedE : (string * typ * exp option * string) IM.map
}

val empty = {
    datatypes = IM.empty,
    constructors = IM.empty,

    relE = [],
    namedE = IM.empty
}

fun pushDatatype (env : env) x n xncs =
    {datatypes = IM.insert (#datatypes env, n, (x, xncs)),
     constructors = foldl (fn ((x, n', to), constructors) =>
                              IM.insert (constructors, n', (x, to, n)))
                          (#constructors env) xncs,

     relE = #relE env,
     namedE = #namedE env}

fun lookupDatatype (env : env) n =
    case IM.find (#datatypes env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupConstructor (env : env) n =
    case IM.find (#constructors env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

structure U = MonoUtil

val liftExpInExp =
    U.Exp.mapB {typ = fn t => t,
                exp = fn bound => fn e =>
                                     case e of
                                         ERel xn =>
                                         if xn < bound then
                                             e
                                         else
                                             ERel (xn + 1)
                                       | _ => e,
                bind = fn (bound, U.Exp.RelE _) => bound + 1
                        | (bound, _) => bound}

val subExpInExp =
    U.Exp.mapB {typ = fn t => t,
                exp = fn (xn, rep) => fn e =>
                                  case e of
                                      ERel xn' =>
                                      (case Int.compare (xn', xn) of
                                           EQUAL => #1 rep
                                         | GREATER=> ERel (xn' - 1)
                                         | LESS => e)
                                    | _ => e,
                bind = fn ((xn, rep), U.Exp.RelE _) => (xn+1, liftExpInExp 0 rep)
                        | (ctx, _) => ctx}

fun pushERel (env : env) x t eo =
    {datatypes = #datatypes env,
     constructors = #constructors env,
     relE = (x, t, eo) :: map (fn (x, t, eo) => (x, t, Option.map (liftExpInExp 0) eo)) (#relE env),
     namedE = #namedE env}

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun pushENamed (env : env) x n t eo s =
    {datatypes = #datatypes env,
     constructors = #constructors env,

     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t, eo, s))}

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun declBinds env (d, loc) =
    case d of
        DDatatype dts =>
        foldl (fn ((x, n, xncs), env) =>
                  let
                      val env = pushDatatype env x n xncs
                      val dt = (TDatatype (n, ref (ElabUtil.classifyDatatype xncs, xncs)), loc)
                  in
                      foldl (fn ((x', n', NONE), env) => pushENamed env x' n' dt NONE ""
                              | ((x', n', SOME t), env) => pushENamed env x' n' (TFun (t, dt), loc) NONE "")
                            env xncs
                  end) env dts
      | DVal (x, n, t, e, s) => pushENamed env x n t (SOME e) s
      | DValRec vis => foldl (fn ((x, n, t, e, s), env) => pushENamed env x n t NONE s) env vis
      | DExport _ => env
      | DTable _ => env
      | DSequence _ => env
      | DView _ => env
      | DIndex _ => env
      | DDatabase _ => env
      | DJavaScript _ => env
      | DCookie _ => env
      | DStyle _ => env
      | DTask _ => env
      | DPolicy _ => env
      | DOnError _ => env

fun patBinds env (p, loc) =
    case p of
        PVar (x, t) => pushERel env x t NONE
      | PPrim _ => env
      | PCon (_, _, NONE) => env
      | PCon (_, _, SOME p) => patBinds env p
      | PRecord xps => foldl (fn ((_, p, _), env) => patBinds env p) env xps
      | PNone _ => env
      | PSome (_, p) => patBinds env p

fun patBindsN (p, loc) =
    case p of
        PVar _ => 1
      | PPrim _ => 0
      | PCon (_, _, NONE) => 0
      | PCon (_, _, SOME p) => patBindsN p
      | PRecord xps => foldl (fn ((_, p, _), count) => count + patBindsN p) 0 xps
      | PNone _ => 0
      | PSome (_, p) => patBindsN p

end
