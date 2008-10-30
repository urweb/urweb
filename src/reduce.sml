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
val subConInCon = E.subConInCon
val liftConInExp = E.liftConInExp
val liftExpInExp = E.liftExpInExp
val subExpInExp = E.subExpInExp
val liftConInExp = E.liftConInExp
val subConInExp = E.subConInExp

fun bindC (env, b) =
    case b of
        U.Con.Rel (x, k) => E.pushCRel env x k
      | U.Con.Named (x, n, k, co) => E.pushCNamed env x n k co

fun bind (env, b) =
    case b of
        U.Decl.RelC (x, k) => E.pushCRel env x k
      | U.Decl.NamedC (x, n, k, co) => E.pushCNamed env x n k co
      | U.Decl.RelE (x, t) => E.pushERel env x t
      | U.Decl.NamedE (x, n, t, eo, s) => E.pushENamed env x n t eo s

fun kind k = k

fun con env c =
    case c of
        CApp ((CApp ((CApp ((CFold ks, _), f), _), i), loc), (CRecord (k, xcs), _)) =>
        (case xcs of
             [] => #1 i
           | (n, v) :: rest =>
             #1 (reduceCon env (CApp ((CApp ((CApp (f, n), loc), v), loc),
                                      (CApp ((CApp ((CApp ((CFold ks, loc), f), loc), i), loc),
                                             (CRecord (k, rest), loc)), loc)), loc)))
      | CApp ((CAbs (_, _, c1), loc), c2) =>
        #1 (reduceCon env (subConInCon (0, c2) c1))
      | CNamed n =>
        (case E.lookupCNamed env n of
             (_, _, SOME c') => #1 c'
           | _ => c)
      | CConcat ((CRecord (k, xcs1), loc), (CRecord (_, xcs2), _)) => CRecord (k, xcs1 @ xcs2)

      | CProj ((CTuple cs, _), n) => #1 (List.nth (cs, n - 1))

      | _ => c

and reduceCon env = U.Con.mapB {kind = kind, con = con, bind = bindC} env

fun exp env e =
    let
        (*val () = Print.prefaces "exp" [("e", CorePrint.p_exp env (e, ErrorMsg.dummySpan))]*)

        val r = case e of
                    ENamed n =>
                    (case E.lookupENamed env n of
                         (_, _, SOME e', _) => #1 e'
                       | _ => e)

                  | ECApp ((EApp ((EApp ((ECApp ((EFold ks, _), ran), _), f), _), i), _), (CRecord (k, xcs), loc)) =>
                    (case xcs of
                         [] => #1 i
                       | (n, v) :: rest =>
                         #1 (reduceExp env (EApp ((ECApp ((ECApp ((ECApp (f, n), loc), v), loc), (CRecord (k, rest), loc)), loc),
                                                  (ECApp ((EApp ((EApp ((ECApp ((EFold ks, loc), ran), loc), f), loc), i), loc),
                                                          (CRecord (k, rest), loc)), loc)), loc)))

                  | EApp ((EAbs (_, _, _, e1), loc), e2) =>
                    #1 (reduceExp env (subExpInExp (0, e2) e1))
                  | ECApp ((ECAbs (_, _, e1), loc), c) =>
                    #1 (reduceExp env (subConInExp (0, c) e1))

                  | EField ((ERecord xes, _), (CName x, _), _) =>
                    (case List.find (fn ((CName x', _), _, _) => x' = x
                                      | _ => false) xes of
                         SOME (_, e, _) => #1 e
                       | NONE => e)
                  | EWith (r as (_, loc), x, e, {rest = (CRecord (k, xts), _), field}) =>
                    let
                        fun fields (remaining, passed) =
                            case remaining of
                                [] => []
                              | (x, t) :: rest =>
                                (x,
                                 (EField (r, x, {field = t,
                                                 rest = (CRecord (k, List.revAppend (passed, rest)), loc)}), loc),
                                 t) :: fields (rest, (x, t) :: passed)
                    in
                        #1 (reduceExp env (ERecord ((x, e, field) :: fields (xts, [])), loc))
                    end
                  | ECut (r as (_, loc), _, {rest = (CRecord (k, xts), _), ...}) =>
                    let
                        fun fields (remaining, passed) =
                            case remaining of
                                [] => []
                              | (x, t) :: rest =>
                                (x,
                                 (EField (r, x, {field = t,
                                                 rest = (CRecord (k, List.revAppend (passed, rest)), loc)}), loc),
                                 t) :: fields (rest, (x, t) :: passed)
                    in
                        #1 (reduceExp env (ERecord (fields (xts, [])), loc))
                    end

                  | _ => e
    in
        (*Print.prefaces "exp'" [("e", CorePrint.p_exp env (e, ErrorMsg.dummySpan)),
                               ("r", CorePrint.p_exp env (r, ErrorMsg.dummySpan))];*)

        r
    end

and reduceExp env = U.Exp.mapB {kind = kind, con = con, exp = exp, bind = bind} env

fun decl env d =
    case d of
        DValRec [vi as (_, n, _, e, _)] =>
        let
            fun kind _ = false
            fun con _ = false
            fun exp e =
                case e of
                    ENamed n' => n' = n
                  | _ => false
        in
            if U.Exp.exists {kind = kind, con = con, exp = exp} e then
                d
            else
                DVal vi
        end
      | _ => d

val reduce = U.File.mapB {kind = kind, con = con, exp = exp, decl = decl, bind = bind} E.empty

end
