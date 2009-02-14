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

structure IM = IntBinaryMap

datatype env_item =
         Unknown
       | Known of exp

       | Lift of int

type env = env_item list

val deKnown = List.filter (fn Known _ => false
                            | _ => true)

fun exp env (all as (e, loc)) =
    case e of
        EPrim _ => all
      | ERel n =>
        let
            fun find (n', env, nudge, lift) =
                case env of
                    [] => raise Fail "ReduceLocal.exp: ERel"
                  | Lift lift' :: rest => find (n', rest, nudge + lift', lift + lift')
                  | Unknown :: rest =>
                    if n' = 0 then
                        (ERel (n + nudge), loc)
                    else
                        find (n' - 1, rest, nudge, lift + 1)
                  | Known e :: rest =>
                    if n' = 0 then
                        ((*print "SUBSTITUTING\n";*)
                         exp (Lift lift :: rest) e)
                    else
                        find (n' - 1, rest, nudge - 1, lift)
        in
            find (n, env, 0, 0)
        end
      | ENamed _ => all
      | ECon (dk, pc, cs, eo) => (ECon (dk, pc, cs, Option.map (exp env) eo), loc)
      | EFfi _ => all
      | EFfiApp (m, f, es) => (EFfiApp (m, f, map (exp env) es), loc)

      | EApp (e1, e2) =>
        let
            val e1 = exp env e1
            val e2 = exp env e2
        in
            case #1 e1 of
                EAbs (_, _, _, b) => exp (Known e2 :: deKnown env) b
              | _ => (EApp (e1, e2), loc)
        end

      | EAbs (x, dom, ran, e) => (EAbs (x, dom, ran, exp (Unknown :: env) e), loc)

      | ECApp (e, c) => (ECApp (exp env e, c), loc)

      | ECAbs (x, k, e) => (ECAbs (x, k, exp env e), loc)

      | ERecord xcs => (ERecord (map (fn (x, e, t) => (x, exp env e, t)) xcs), loc)
      | EField (e, c, others) =>
        let
            val e = exp env e

            fun default () = (EField (e, c, others), loc)
        in
            case (#1 e, #1 c) of
                (ERecord xcs, CName x) =>
                (case List.find (fn ((CName x', _), _, _) => x' = x | _ => false) xcs of
                     NONE => default ()
                   | SOME (_, e, _) => e)
              | _ => default ()
        end

      | EConcat (e1, c1, e2, c2) => (EConcat (exp env e1, c1, exp env e2, c2), loc)
      | ECut (e, c, others) => (ECut (exp env e, c, others), loc)
      | ECutMulti (e, c, others) => (ECutMulti (exp env e, c, others), loc)

      | EFold _ => all

      | ECase (e, pes, others) =>
        let
            fun patBinds (p, _) =
                case p of
                    PWild => 0
                  | PVar _ => 1
                  | PPrim _ => 0
                  | PCon (_, _, _, NONE) => 0
                  | PCon (_, _, _, SOME p) => patBinds p
                  | PRecord xpts => foldl (fn ((_, p, _), n) => n + patBinds p) 0 xpts
        in
            (ECase (exp env e,
                    map (fn (p, e) => (p,
                                       exp (List.tabulate (patBinds p, fn _ => Unknown) @ env) e))
                        pes, others), loc)
        end

      | EWrite e => (EWrite (exp env e), loc)
      | EClosure (n, es) => (EClosure (n, map (exp env) es), loc)

      | ELet (x, t, e1, e2) => (ELet (x, t, exp env e1, exp (Unknown :: env) e2), loc)

      | EServerCall (n, es, e) => (EServerCall (n, map (exp env) es, exp env e), loc)

fun reduce file =
    let
        fun doDecl (d as (_, loc)) =
            case #1 d of
                DCon _ => d
              | DDatatype _ => d
              | DVal (x, n, t, e, s) =>
                let
                    val e = exp [] e
                in
                    (DVal (x, n, t, e, s), loc)
                end
              | DValRec vis =>
                (DValRec (map (fn (x, n, t, e, s) => (x, n, t, exp [] e, s)) vis), loc)
              | DExport _ => d
              | DTable _ => d
              | DSequence _ => d
              | DDatabase _ => d
              | DCookie _ => d
    in
        map doDecl file
    end

end
