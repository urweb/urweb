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

(* Simplify a Core program algebraically *)

structure Reduce :> REDUCE = struct

open Core

structure E = CoreEnv
structure U = CoreUtil

val liftConInCon = E.liftConInCon

val subConInCon =
    U.Con.mapB {kind = fn k => k,
                con = fn (xn, rep) => fn c =>
                                  case c of
                                      CRel xn' =>
                                      if xn = xn' then
                                          #1 rep
                                      else
                                          c
                                    | _ => c,
                bind = fn ((xn, rep), U.Con.Rel _) => (xn+1, liftConInCon 0 rep)
                        | (ctx, _) => ctx}

val liftExpInExp =
    U.Exp.mapB {kind = fn k => k,
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
    U.Exp.mapB {kind = fn k => k,
                con = fn _ => fn c => c,
                exp = fn (xn, rep) => fn e =>
                                  case e of
                                      ERel xn' =>
                                      if xn = xn' then
                                          #1 rep
                                      else
                                          e
                                    | _ => e,
                bind = fn ((xn, rep), U.Exp.RelE _) => (xn+1, liftExpInExp 0 rep)
                        | (ctx, _) => ctx}

val liftConInExp =
    U.Exp.mapB {kind = fn k => k,
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
    U.Exp.mapB {kind = fn k => k,
                con = fn (xn, rep) => fn c =>
                                  case c of
                                      CRel xn' =>
                                      if xn = xn' then
                                          #1 rep
                                      else
                                          c
                                    | _ => c,
                exp = fn _ => fn e => e,
                bind = fn ((xn, rep), U.Exp.RelC _) => (xn+1, liftConInCon 0 rep)
                        | (ctx, _) => ctx}

fun bindC (env, b) =
    case b of
        U.Con.Rel (x, k) => E.pushCRel env x k
      | U.Con.Named (x, n, k, co) => E.pushCNamed env x n k co

fun bind (env, b) =
    case b of
        U.Decl.RelC (x, k) => E.pushCRel env x k
      | U.Decl.NamedC (x, n, k, co) => E.pushCNamed env x n k co
      | U.Decl.RelE (x, t) => E.pushERel env x t
      | U.Decl.NamedE (x, n, t, eo) => E.pushENamed env x n t eo

fun kind k = k

fun con env c =
    case c of
        CApp ((CAbs (_, _, c1), loc), c2) =>
        #1 (reduceCon env (subConInCon (0, c2) c1))
      | CNamed n =>
        (case E.lookupCNamed env n of
             (_, _, SOME c') => #1 c'
           | _ => c)
      | CConcat ((CRecord (k, xcs1), loc), (CRecord (_, xcs2), _)) => CRecord (k, xcs1 @ xcs2)
      | _ => c

and reduceCon env = U.Con.mapB {kind = kind, con = con, bind = bindC} env

fun exp env e =
    case e of
        ENamed n =>
        (case E.lookupENamed env n of
             (_, _, SOME e') => #1 e'
           | _ => e)

      | EApp ((EAbs (_, _, e1), loc), e2) =>
        #1 (reduceExp env (subExpInExp (0, e2) e1))
      | ECApp ((ECAbs (_, _, e1), loc), c) =>
        #1 (reduceExp env (subConInExp (0, c) e1))

      | _ => e

and reduceExp env = U.Exp.mapB {kind = kind, con = con, exp = exp, bind = bind} env

fun decl env d = d

val reduce = U.File.mapB {kind = kind, con = con, exp = exp, decl = decl, bind = bind} CoreEnv.basis

end
