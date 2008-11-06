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

val liftConInCon =
    U.Con.mapB {kind = fn k => k,
                con = fn bound => fn c =>
                                     case c of
                                         CRel xn =>
                                         if xn < bound then
                                             c
                                         else
                                             CRel (xn + 1)
                                       (*| CUnif _ => raise SynUnif*)
                                       | _ => c,
                bind = fn (bound, U.Con.Rel _) => bound + 1
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
     renameC : kind var' SM.map,
     relC : (string * kind) list,
     namedC : (string * kind * con option) IM.map,

     renameE : con var' SM.map,
     relE : (string * con) list,
     namedE : (string * con) IM.map,

     renameSgn : (int * sgn) SM.map,
     sgn : (string * sgn) IM.map,

     renameStr : (int * sgn) SM.map,
     str : (string * sgn) IM.map
}

val namedCounter = ref 0

val empty = {
    renameC = SM.empty,
    relC = [],
    namedC = IM.empty,

    renameE = SM.empty,
    relE = [],
    namedE = IM.empty,

    renameSgn = SM.empty,
    sgn = IM.empty,

    renameStr = SM.empty,
    str = IM.empty
}

fun pushCRel (env : env) x k =
    let
        val renameC = SM.map (fn Rel' (n, k) => Rel' (n+1, k)
                               | x => x) (#renameC env)
    in
        {renameC = SM.insert (renameC, x, Rel' (0, k)),
         relC = (x, k) :: #relC env,
         namedC = IM.map (fn (x, k, co) => (x, k, Option.map lift co)) (#namedC env),

         renameE = #renameE env,
         relE = map (fn (x, c) => (x, lift c)) (#relE env),
         namedE = IM.map (fn (x, c) => (x, lift c)) (#namedE env),

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env
        }
    end

fun lookupCRel (env : env) n =
    (List.nth (#relC env, n))
    handle Subscript => raise UnboundRel n

fun pushCNamed (env : env) x n k co =
    {renameC = SM.insert (#renameC env, x, Named' (n, k)),
     relC = #relC env,
     namedC = IM.insert (#namedC env, n, (x, k, co)),

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = #renameSgn env,
     sgn = #sgn env,
     
     renameStr = #renameStr env,
     str = #str env}

fun lookupCNamed (env : env) n =
    case IM.find (#namedC env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushERel (env : env) x t =
    let
        val renameE = SM.map (fn Rel' (n, t) => Rel' (n+1, t)
                               | x => x) (#renameE env)
    in
        {renameC = #renameC env,
         relC = #relC env,
         namedC = #namedC env,

         renameE = SM.insert (renameE, x, Rel' (0, t)),
         relE = (x, t) :: #relE env,
         namedE = #namedE env,

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env}
    end

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun pushENamed (env : env) x n t =
    {renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     renameE = SM.insert (#renameE env, x, Named' (n, t)),
     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t)),

     renameSgn = #renameSgn env,
     sgn = #sgn env,
     
     renameStr = #renameStr env,
     str = #str env}

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushSgnNamed (env : env) x n sgis =
    {renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = SM.insert (#renameSgn env, x, (n, sgis)),
     sgn = IM.insert (#sgn env, n, (x, sgis)),
     
     renameStr = #renameStr env,
     str = #str env}

fun lookupSgnNamed (env : env) n =
    case IM.find (#sgn env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun pushStrNamed (env : env) x n sgis =
    {renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = #renameSgn env,
     sgn = #sgn env,

     renameStr = SM.insert (#renameStr env, x, (n, sgis)),
     str = IM.insert (#str env, n, (x, sgis))}

fun lookupStrNamed (env : env) n =
    case IM.find (#str env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun declBinds env (d, loc) =
    case d of
        DCon (x, n, k, c) => pushCNamed env x n k (SOME c)
      | DDatatype (x, n, xs, xncs) =>
        let
            val env = pushCNamed env x n (KType, loc) NONE
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
      | DTable (tn, x, n, c) =>
        let
            val t = (CApp ((CModProj (tn, [], "sql_table"), loc), c), loc)
        in
            pushENamed env x n t
        end
      | DSequence (tn, x, n) =>
        let
            val t = (CModProj (tn, [], "sql_sequence"), loc)
        in
            pushENamed env x n t
        end
      | DDatabase _ => env
      | DCookie (tn, x, n, c) =>
        let
            val t = (CApp ((CModProj (tn, [], "http_cookie"), loc), c), loc)
        in
            pushENamed env x n t
        end

fun sgiBinds env (sgi, loc) =
    case sgi of
        SgiConAbs (x, n, k) => pushCNamed env x n k NONE
      | SgiCon (x, n, k, c) => pushCNamed env x n k (SOME c)
      | SgiDatatype (x, n, xs, xncs) =>
        let
            val env = pushCNamed env x n (KType, loc) NONE
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

end
