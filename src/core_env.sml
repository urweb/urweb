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

val liftKindInKind =
    U.Kind.mapB {kind = fn bound => fn k =>
                                       case k of
                                           KRel xn =>
                                           if xn < bound then
                                               k
                                           else
                                               KRel (xn + 1)
                                         | _ => k,
                 bind = fn (bound, _) => bound + 1}

val liftKindInCon =
    U.Con.mapB {kind = fn bound => fn k =>
                                      case k of
                                          KRel xn =>
                                          if xn < bound then
                                              k
                                          else
                                              KRel (xn + 1)
                                        | _ => k,
                con = fn _ => fn c => c,
                bind = fn (bound, U.Con.RelK _) => bound + 1
                        | (bound, _) => bound}

val liftKindInExp =
    U.Exp.mapB {kind = fn bound => fn k =>
                                      case k of
                                          KRel xn =>
                                          if xn < bound then
                                              k
                                          else
                                              KRel (xn + 1)
                                        | _ => k,
                con = fn _ => fn c => c,
                exp = fn _ => fn e => e,
                bind = fn (bound, U.Exp.RelK _) => bound + 1
                        | (bound, _) => bound}
    
val liftConInCon =
    U.Con.mapB {kind = fn _ => fn k => k,
                con = fn bound => fn c =>
                                     case c of
                                         CRel xn =>
                                         if xn < bound then
                                             c
                                         else
                                             CRel (xn + 1)
                                       | _ => c,
                bind = fn (bound, U.Con.RelC _) => bound + 1
                        | (bound, _) => bound}

val lift = liftConInCon 0

val subConInCon =
    U.Con.mapB {kind = fn _ => fn k => k,
                con = fn (xn, rep) => fn c =>
                                  case c of
                                      CRel xn' =>
                                      (case Int.compare (xn', xn) of
                                           EQUAL => #1 rep
                                         | GREATER => CRel (xn' - 1)
                                         | LESS => c)
                                    | _ => c,
                bind = fn ((xn, rep), U.Con.RelC _) => (xn+1, liftConInCon 0 rep)
                        | (ctx, _) => ctx}


val liftConInExp =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn bound => fn c =>
                                     case c of
                                         CRel xn =>
                                         if xn < bound then
                                             c
                                         else
                                             CRel (xn + 1)
                                       | _ => c,
                exp = fn _ => fn e => e,
                bind = fn (bound, U.Exp.RelC _) => bound + 1
                        | (bound, _) => bound}

val subConInExp =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn (xn, rep) => fn c =>
                                         case c of
                                             CRel xn' =>
                                             (case Int.compare (xn', xn) of
                                                  EQUAL => #1 rep
                                                | GREATER => CRel (xn' - 1)
                                                | LESS => c)
                                           | _ => c,
                exp = fn _ => fn e => e,
                bind = fn ((xn, rep), U.Exp.RelC _) => (xn+1, liftConInCon 0 rep)
                        | (ctx, _) => ctx}

val liftExpInExp =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn _ => fn c => c,
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
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn _ => fn c => c,
                exp = fn (xn, rep) => fn e =>
                                  case e of
                                      ERel xn' =>
                                      (case Int.compare (xn', xn) of
                                           EQUAL => #1 rep
                                         | GREATER=> ERel (xn' - 1)
                                         | LESS => e)
                                    | _ => e,
                bind = fn ((xn, rep), U.Exp.RelE _) => (xn+1, liftExpInExp 0 rep)
                        | ((xn, rep), U.Exp.RelC _) => (xn, liftConInExp 0 rep)
                        | (ctx, _) => ctx}

(* Back to environments *)

exception UnboundRel of int
exception UnboundNamed of int

type env = {
     relK : string list,

     relC : (string * kind) list,
     namedC : (string * kind * con option) IM.map,

     datatypes : (string * string list * (string * int * con option) list) IM.map,
     constructors : (string * string list * con option * int) IM.map,

     relE : (string * con) list,
     namedE : (string * con * exp option * string) IM.map
}

val empty = {
    relK = [],

    relC = [],
    namedC = IM.empty,

    datatypes = IM.empty,
    constructors = IM.empty,

    relE = [],
    namedE = IM.empty
}

fun pushKRel (env : env) x =
    {relK = x :: #relK env,

     relC = map (fn (x, k) => (x, liftKindInKind 0 k)) (#relC env),
     namedC = #namedC env,

     relE = map (fn (x, c) => (x, liftKindInCon 0 c)) (#relE env),
     namedE = #namedE env,

     datatypes = #datatypes env,
     constructors = #constructors env
    }

fun lookupKRel (env : env) n =
    (List.nth (#relK env, n))
    handle Subscript => raise UnboundRel n

fun pushCRel (env : env) x k =
    {relK = #relK env,

     relC = (x, k) :: #relC env,
     namedC = IM.map (fn (x, k, co) => (x, k, Option.map lift co)) (#namedC env),

     datatypes = #datatypes env,
     constructors = #constructors env,

     relE = map (fn (x, c) => (x, lift c)) (#relE env),
     namedE = IM.map (fn (x, c, eo, s) => (x, lift c, eo, s)) (#namedE env)}

fun lookupCRel (env : env) n =
    (List.nth (#relC env, n))
    handle Subscript => raise UnboundRel n

fun pushCNamed (env : env) x n k co =
    {relK = #relK env,

     relC = #relC env,
     namedC = IM.insert (#namedC env, n, (x, k, co)),

     datatypes = #datatypes env,
     constructors = #constructors env,
     
     relE = #relE env,
     namedE = #namedE env}

fun lookupCNamed (env : env) n =
    case IM.find (#namedC env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushDatatype (env : env) x n xs xncs =
    {relK = #relK env,

     relC = #relC env,
     namedC = #namedC env,

     datatypes = IM.insert (#datatypes env, n, (x, xs, xncs)),
     constructors = foldl (fn ((x, n', to), constructors) =>
                              IM.insert (constructors, n', (x, xs, to, n)))
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

fun pushERel (env : env) x t =
    {relK = #relK env,

     relC = #relC env,
     namedC = #namedC env,

     datatypes = #datatypes env,
     constructors = #constructors env,

     relE = (x, t) :: #relE env,
     namedE = #namedE env}

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun pushENamed (env : env) x n t eo s =
    {relK = #relK env,

     relC = #relC env,
     namedC = #namedC env,

     datatypes = #datatypes env,
     constructors = #constructors env,

     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t, eo, s))}

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun declBinds env (d, loc) =
    case d of
        DCon (x, n, k, c) => pushCNamed env x n k (SOME c)
      | DDatatype dts =>
        foldl (fn ((x, n, xs, xncs), env) =>
                  let
                      val env = pushDatatype env x n xs xncs
                      val env = pushCNamed env x n (KType, loc) NONE
                  in
                      foldl (fn ((x', n', NONE), env) => pushENamed env x' n' (CNamed n, loc) NONE ""
                              | ((x', n', SOME t), env) => pushENamed env x' n' (TFun (t, (CNamed n, loc)), loc) NONE "")
                            env xncs
                  end) env dts
      | DVal (x, n, t, e, s) => pushENamed env x n t (SOME e) s
      | DValRec vis => foldl (fn ((x, n, t, e, s), env) => pushENamed env x n t NONE s) env vis
      | DExport _ => env
      | DTable (x, n, c, s, _, pc, _, cc) =>
        let
            val ct = (CFfi ("Basis", "sql_table"), loc)
            val ct = (CApp (ct, c), loc)
            val ct = (CApp (ct, (CConcat (pc, cc), loc)), loc)
        in
            pushENamed env x n ct NONE s
        end
      | DSequence (x, n, s) =>
        let
            val t = (CFfi ("Basis", "sql_sequence"), loc)
        in
            pushENamed env x n t NONE s
        end
      | DView (x, n, s, _, c) =>
        let
            val ct = (CFfi ("Basis", "sql_view"), loc)
            val ct = (CApp (ct, c), loc)
        in
            pushENamed env x n ct NONE s
        end
      | DDatabase _ => env
      | DCookie (x, n, c, s) =>
        let
            val t = (CApp ((CFfi ("Basis", "http_cookie"), loc), c), loc)
        in
            pushENamed env x n t NONE s
        end
      | DStyle (x, n, s) =>
        let
            val t = (CFfi ("Basis", "css_class"), loc)
        in
            pushENamed env x n t NONE s
        end
      | DTask _ => env
      | DPolicy _ => env
      | DOnError _ => env

fun patBinds env (p, loc) =
    case p of
        PWild => env
      | PVar (x, t) => pushERel env x t
      | PPrim _ => env
      | PCon (_, _, _, NONE) => env
      | PCon (_, _, _, SOME p) => patBinds env p
      | PRecord xps => foldl (fn ((_, p, _), env) => patBinds env p) env xps

fun patBindsN (p, loc) =
    case p of
        PWild => 0
      | PVar _ => 1
      | PPrim _ => 0
      | PCon (_, _, _, NONE) => 0
      | PCon (_, _, _, SOME p) => patBindsN p
      | PRecord xps => foldl (fn ((_, p, _), count) => count + patBindsN p) 0 xps

fun patBindsL (p, loc) =
    case p of
        PWild => []
      | PVar (x, t) => [(x, t)]
      | PPrim _ => []
      | PCon (_, _, _, NONE) => []
      | PCon (_, _, _, SOME p) => patBindsL p
      | PRecord xps => rev (ListUtil.mapConcat (rev o patBindsL o #2) xps)

end
