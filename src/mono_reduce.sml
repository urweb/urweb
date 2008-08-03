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

(* Simplify a Mono program algebraically *)

structure MonoReduce :> MONO_REDUCE = struct

open Mono

structure E = MonoEnv
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

fun bind (env, b) =
    case b of
        U.Decl.Datatype (x, n, xncs) => E.pushDatatype env x n xncs
      | U.Decl.RelE (x, t) => E.pushERel env x t NONE
      | U.Decl.NamedE (x, n, t, eo, s) => E.pushENamed env x n t eo s

fun typ c = c

fun match (env, p : pat, e : exp) =
    case (#1 p, #1 e) of
        (PWild, _) => SOME env
      | (PVar (x, t), _) => SOME (E.pushERel env x t (SOME e))

      | (PPrim p, EPrim p') =>
        if Prim.equal (p, p') then
            SOME env
        else
            NONE

      | (PCon (_, PConVar n1, NONE), ECon (_, PConVar n2, NONE)) =>
        if n1 = n2 then
            SOME env
        else
            NONE

      | (PCon (_, PConVar n1, SOME p), ECon (_, PConVar n2, SOME e)) =>
        if n1 = n2 then
            match (env, p, e)
        else
            NONE

      | (PCon (_, PConFfi {mod = m1, con = con1, ...}, NONE), ECon (_, PConFfi {mod = m2, con = con2, ...}, NONE)) =>
        if m1 = m2 andalso con1 = con2 then
            SOME env
        else
            NONE

      | (PCon (_, PConFfi {mod = m1, con = con1, ...}, SOME ep), ECon (_, PConFfi {mod = m2, con = con2, ...}, SOME e)) =>
        if m1 = m2 andalso con1 = con2 then
            match (env, p, e)
        else
            NONE

      | (PRecord xps, ERecord xes) =>
        let
            fun consider (xps, env) =
                case xps of
                    [] => SOME env
                  | (x, p, _) :: rest =>
                    case List.find (fn (x', _, _) => x' = x) xes of
                        NONE => NONE
                      | SOME (_, e, _) =>
                        case match (env, p, e) of
                            NONE => NONE
                          | SOME env => consider (rest, env)
        in
            consider (xps, env)
        end

      | _ => NONE

fun exp env e =
    case e of
        ERel n =>
        (case E.lookupERel env n of
             (_, _, SOME e') => #1 e'
           | _ => e)
      | ENamed n =>
        (case E.lookupENamed env n of
             (_, _, SOME e', _) => #1 e'
           | _ => e)

      | EApp ((EAbs (_, _, _, e1), loc), e2) =>
        #1 (reduceExp env (subExpInExp (0, e2) e1))

      | ECase (disc, pes, _) =>
        (case ListUtil.search (fn (p, body) =>
                                  case match (env, p, disc) of
                                      NONE => NONE
                                    | SOME env => SOME (#1 (reduceExp env body))) pes of
             NONE => e
           | SOME e' => e')

      | _ => e

and reduceExp env = U.Exp.mapB {typ = typ, exp = exp, bind = bind} env

fun decl env d = d

val reduce = U.File.mapB {typ = typ, exp = exp, decl = decl, bind = bind} E.empty

end
