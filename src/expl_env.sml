(* Copyright (c) 2008-2010, Adam Chlipala
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

structure ExplEnv :> EXPL_ENV = struct

open Expl

structure U = ExplUtil

structure IM = IntBinaryMap
structure SM = BinaryMapFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

exception UnboundRel of int
exception UnboundNamed of int


(* AST utility functions *)

exception SynUnif

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

val liftConInCon =
    U.Con.mapB {kind = fn _ => fn k => k,
                con = fn bound => fn c =>
                                     case c of
                                         CRel xn =>
                                         if xn < bound then
                                             c
                                         else
                                             CRel (xn + 1)
                                       (*| CUnif _ => raise SynUnif*)
                                       | _ => c,
                bind = fn (bound, U.Con.RelC _) => bound + 1
                        | (bound, _) => bound}

val lift = liftConInCon 0


(* Back to environments *)

datatype 'a var' =
         Rel' of int * 'a
       | Named' of int * 'a

datatype 'a var =
         NotBound
       | Rel of int * 'a
       | Named of int * 'a

type env = {
     relK : string list,

     relC : (string * kind) list,
     namedC : (string * kind * con option) IM.map,

     relE : (string * con) list,
     namedE : (string * con) IM.map,

     sgn : (string * sgn) IM.map,

     str : (string * sgn) IM.map
}

val namedCounter = ref 0

val empty = {
    relK = [],

    relC = [],
    namedC = IM.empty,

    relE = [],
    namedE = IM.empty,

    sgn = IM.empty,

    str = IM.empty
}

fun pushKRel (env : env) x =
    {relK = x :: #relK env,

     relC = map (fn (x, k) => (x, liftKindInKind 0 k)) (#relC env),
     namedC = #namedC env,

     relE = map (fn (x, c) => (x, liftKindInCon 0 c)) (#relE env),
     namedE = #namedE env,

     sgn = #sgn env,

     str = #str env
    }

fun lookupKRel (env : env) n =
    (List.nth (#relK env, n))
    handle Subscript => raise UnboundRel n

fun pushCRel (env : env) x k =
    {relK = #relK env,

     relC = (x, k) :: #relC env,
     namedC = IM.map (fn (x, k, co) => (x, k, Option.map lift co)) (#namedC env),

     relE = map (fn (x, c) => (x, lift c)) (#relE env),
     namedE = IM.map (fn (x, c) => (x, lift c)) (#namedE env),

     sgn = #sgn env,

     str = #str env
    }

fun lookupCRel (env : env) n =
    (List.nth (#relC env, n))
    handle Subscript => raise UnboundRel n

fun pushCNamed (env : env) x n k co =
    {relK = #relK env,

     relC = #relC env,
     namedC = IM.insert (#namedC env, n, (x, k, co)),

     relE = #relE env,
     namedE = #namedE env,

     sgn = #sgn env,
     
     str = #str env}

fun lookupCNamed (env : env) n =
    case IM.find (#namedC env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushERel (env : env) x t =
    {relK = #relK env,

     relC = #relC env,
     namedC = #namedC env,

     relE = (x, t) :: #relE env,
     namedE = #namedE env,

     sgn = #sgn env,

     str = #str env}

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun pushENamed (env : env) x n t =
    {relK = #relK env,

     relC = #relC env,
     namedC = #namedC env,

     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t)),

     sgn = #sgn env,
     
     str = #str env}

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushSgnNamed (env : env) x n sgis =
    {relK = #relK env,

     relC = #relC env,
     namedC = #namedC env,

     relE = #relE env,
     namedE = #namedE env,

     sgn = IM.insert (#sgn env, n, (x, sgis)),
     
     str = #str env}

fun lookupSgnNamed (env : env) n =
    case IM.find (#sgn env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushStrNamed (env : env) x n sgis =
    {relK = #relK env,

     relC = #relC env,
     namedC = #namedC env,

     relE = #relE env,
     namedE = #namedE env,

     sgn = #sgn env,

     str = IM.insert (#str env, n, (x, sgis))}

fun lookupStrNamed (env : env) n =
    case IM.find (#str env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun declBinds env (d, loc) =
    case d of
        DCon (x, n, k, c) => pushCNamed env x n k (SOME c)
      | DDatatype dts =>
        let
            fun doOne ((x, n, xs, xncs), env) =
                let
                    val k = (KType, loc) 
                    val nxs = length xs
                    val (tb, kb) = ListUtil.foldli (fn (i, x', (tb, kb)) =>
                                                       ((CApp (tb, (CRel (nxs - i - 1), loc)), loc),
                                                        (KArrow (k, kb), loc)))
                                                   ((CNamed n, loc), k) xs
                                   
                    val env = pushCNamed env x n kb NONE
                in
                    foldl (fn ((x', n', to), env) =>
                              let
                                  val t =
                                      case to of
                                          NONE => tb
                                        | SOME t => (TFun (t, tb), loc)
                                  val t = foldr (fn (x, t) => (TCFun (x, k, t), loc)) t xs
                              in
                                  pushENamed env x' n' t
                              end)
                          env xncs
                end
        in
            foldl doOne env dts
        end
      | DDatatypeImp (x, n, m, ms, x', xs, xncs) =>
        let
            val t = (CModProj (m, ms, x'), loc)
            val env = pushCNamed env x n (KType, loc) (SOME t)

            val t = (CNamed n, loc)
        in
            foldl (fn ((x', n', to), env) =>
                      let
                          val t =
                              case to of
                                  NONE => (CNamed n, loc)
                                | SOME t => (TFun (t, (CNamed n, loc)), loc)
                          val k = (KType, loc)
                          val t = foldr (fn (x, t) => (TCFun (x, k, t), loc)) t xs
                      in
                          pushENamed env x' n' t
                      end)
                  env xncs
        end
      | DVal (x, n, t, _) => pushENamed env x n t
      | DValRec vis => foldl (fn ((x, n, t, _), env) => pushENamed env x n t) env vis
      | DSgn (x, n, sgn) => pushSgnNamed env x n sgn
      | DStr (x, n, sgn, _) => pushStrNamed env x n sgn
      | DFfiStr (x, n, sgn) => pushStrNamed env x n sgn
      | DExport _ => env
      | DTable (tn, x, n, c, _, pc, _, cc) =>
        let
            val ct = (CModProj (tn, [], "sql_table"), loc)
            val ct = (CApp (ct, c), loc)
            val ct = (CApp (ct, (CConcat (pc, cc), loc)), loc)
        in
            pushENamed env x n ct
        end
      | DSequence (tn, x, n) =>
        let
            val t = (CModProj (tn, [], "sql_sequence"), loc)
        in
            pushENamed env x n t
        end
      | DView (tn, x, n, _, c) =>
        let
            val ct = (CModProj (tn, [], "sql_view"), loc)
            val ct = (CApp (ct, c), loc)
        in
            pushENamed env x n ct
        end
      | DDatabase _ => env
      | DCookie (tn, x, n, c) =>
        let
            val t = (CApp ((CModProj (tn, [], "http_cookie"), loc), c), loc)
        in
            pushENamed env x n t
        end
      | DStyle (tn, x, n) =>
        let
            val t = (CModProj (tn, [], "css_class"), loc)
        in
            pushENamed env x n t
        end
      | DTask _ => env
      | DPolicy _ => env
      | DOnError _ => env

fun sgiBinds env (sgi, loc) =
    case sgi of
        SgiConAbs (x, n, k) => pushCNamed env x n k NONE
      | SgiCon (x, n, k, c) => pushCNamed env x n k (SOME c)
      | SgiDatatype dts =>
        let
            fun doOne ((x, n, xs, xncs), env) =
                let
                    val k = (KType, loc)
                    val k' = foldr (fn (_, k') => (KArrow (k, k'), loc)) k xs

                    val env = pushCNamed env x n k' NONE
                in
                    foldl (fn ((x', n', to), env) =>
                              let
                                  val t =
                                      case to of
                                          NONE => (CNamed n, loc)
                                        | SOME t => (TFun (t, (CNamed n, loc)), loc)

                                  val k = (KType, loc)
                                  val t = foldr (fn (x, t) => (TCFun (x, k, t), loc)) t xs
                              in
                                  pushENamed env x' n' t
                              end)
                          env xncs
                end
        in
            foldl doOne env dts
        end
      | SgiDatatypeImp (x, n, m1, ms, x', xs, xncs) =>
        let
            val t = (CModProj (m1, ms, x'), loc)
            val env = pushCNamed env x n (KType, loc) (SOME t)

            val t = (CNamed n, loc)
        in
            foldl (fn ((x', n', to), env) =>
                      let
                          val t =
                              case to of
                                  NONE => (CNamed n, loc)
                                | SOME t => (TFun (t, (CNamed n, loc)), loc)
                          val k = (KType, loc)
                          val t = foldr (fn (x, t) => (TCFun (x, k, t), loc)) t xs
                      in
                          pushENamed env x' n' t
                      end)
                  env xncs
        end
      | SgiVal (x, n, t) => pushENamed env x n t
      | SgiSgn (x, n, sgn) => pushSgnNamed env x n sgn
      | SgiStr (x, n, sgn) => pushStrNamed env x n sgn

fun patBinds env (p, loc) =
    case p of
        PWild => env
      | PVar (x, t) => pushERel env x t
      | PPrim _ => env
      | PCon (_, _, _, NONE) => env
      | PCon (_, _, _, SOME p) => patBinds env p
      | PRecord xps => foldl (fn ((_, p, _), env) => patBinds env p) env xps

end
