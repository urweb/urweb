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


fun impure (e, _) =
    case e of
        EWrite _ => true
      | EQuery _ => true
      | EDml _ => true
      | EAbs _ => false

      | EPrim _ => false
      | ERel _ => false
      | ENamed _ => false
      | ECon (_, _, eo) => (case eo of NONE => false | SOME e => impure e)
      | ENone _ => false
      | ESome (_, e) => impure e
      | EFfi _ => false
      | EFfiApp _ => false
      | EApp ((EFfi _, _), _) => false
      | EApp _ => true

      | ERecord xes => List.exists (fn (_, e, _) => impure e) xes
      | EField (e, _) => impure e

      | ECase (e, pes, _) => impure e orelse List.exists (fn (_, e) => impure e) pes

      | EError (e, _) => impure e

      | EStrcat (e1, e2) => impure e1 orelse impure e2

      | ESeq (e1, e2) => impure e1 orelse impure e2
      | ELet (_, _, e1, e2) => impure e1 orelse impure e2

      | EClosure (_, es) => List.exists impure es


val liftExpInExp = Monoize.liftExpInExp

val subExpInExp' =
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

fun subExpInExp (n, e1) e2 =
    let
        val r = subExpInExp' (n, e1) e2
    in
        (*Print.prefaces "subExpInExp" [("e1", MonoPrint.p_exp MonoEnv.empty e1),
                                      ("e2", MonoPrint.p_exp MonoEnv.empty e2),
                                      ("r", MonoPrint.p_exp MonoEnv.empty r)];*)
        r
    end

fun typ c = c

val swapExpVars =
    U.Exp.mapB {typ = fn t => t,
                exp = fn lower => fn e =>
                                     case e of
                                         ERel xn =>
                                         if xn = lower then
                                             ERel (lower + 1)
                                         else if xn = lower + 1 then
                                             ERel lower
                                         else
                                             e
                                       | _ => e,
                bind = fn (lower, U.Exp.RelE _) => lower+1
                        | (lower, _) => lower}

datatype result = Yes of E.env | No | Maybe

fun match (env, p : pat, e : exp) =
    case (#1 p, #1 e) of
        (PWild, _) => Yes env
      | (PVar (x, t), _) => Yes (E.pushERel env x t (SOME e))

      | (PPrim (Prim.String s), EStrcat ((EPrim (Prim.String s'), _), _)) =>
        if String.isPrefix s' s then
            Maybe
        else
            No

      | (PPrim p, EPrim p') =>
        if Prim.equal (p, p') then
            Yes env
        else
            No

      | (PCon (_, PConVar n1, NONE), ECon (_, PConVar n2, NONE)) =>
        if n1 = n2 then
            Yes env
        else
            No

      | (PCon (_, PConVar n1, SOME p), ECon (_, PConVar n2, SOME e)) =>
        if n1 = n2 then
            match (env, p, e)
        else
            No

      | (PCon (_, PConFfi {mod = m1, con = con1, ...}, NONE), ECon (_, PConFfi {mod = m2, con = con2, ...}, NONE)) =>
        if m1 = m2 andalso con1 = con2 then
            Yes env
        else
            No

      | (PCon (_, PConFfi {mod = m1, con = con1, ...}, SOME ep), ECon (_, PConFfi {mod = m2, con = con2, ...}, SOME e)) =>
        if m1 = m2 andalso con1 = con2 then
            match (env, p, e)
        else
            No

      | (PRecord xps, ERecord xes) =>
        let
            fun consider (xps, env) =
                case xps of
                    [] => Yes env
                  | (x, p, _) :: rest =>
                    case List.find (fn (x', _, _) => x' = x) xes of
                        NONE => No
                      | SOME (_, e, _) =>
                        case match (env, p, e) of
                            No => No
                          | Maybe => Maybe
                          | Yes env => consider (rest, env)
        in
            consider (xps, env)
        end

      | _ => Maybe

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

      | EApp ((EAbs (x, t, _, e1), loc), e2) =>
        ((*Print.prefaces "Considering" [("e1", MonoPrint.p_exp env e1),
                                       ("e2", MonoPrint.p_exp env e2)];*)
        if impure e2 then
            #1 (reduceExp env (ELet (x, t, e2, e1), loc))
        else
            #1 (reduceExp env (subExpInExp (0, e2) e1)))

      | ECase (disc, pes, _) =>
        let
            fun search pes =
                case pes of
                    [] => e
                  | (p, body) :: pes =>
                    case match (env, p, disc) of
                        No => search pes
                      | Maybe => e
                      | Yes env => #1 (reduceExp env body)
        in
            search pes
        end

      | EField ((ERecord xes, _), x) =>
        (case List.find (fn (x', _, _) => x' = x) xes of
             SOME (_, e, _) => #1 e
           | NONE => e)

      | ELet (x1, t1, (ELet (x2, t2, e1, b1), loc), b2) =>
        let
            val e' = (ELet (x2, t2, e1,
                            (ELet (x1, t1, b1,
                                   liftExpInExp 1 b2), loc)), loc)
        in
            (*Print.prefaces "ELet commute" [("e", MonoPrint.p_exp env (e, loc)),
                                           ("e'", MonoPrint.p_exp env e')];*)
            #1 (reduceExp env e')
        end
      | EApp ((ELet (x, t, e, b), loc), e') =>
        #1 (reduceExp env (ELet (x, t, e,
                                 (EApp (b, liftExpInExp 0 e'), loc)), loc))

      | ELet (x, t, e, (EAbs (x', t' as (TRecord [], _), ran, e'), loc)) =>
        EAbs (x', t', ran, (ELet (x, t, liftExpInExp 0 e, swapExpVars 0 e'), loc))

      | ELet (x, t, e', b) =>
        if impure e' then
            e
        else
            #1 (reduceExp env (subExpInExp (0, e') b))

      | EStrcat ((EPrim (Prim.String s1), _), (EPrim (Prim.String s2), _)) =>
        EPrim (Prim.String (s1 ^ s2))

      | _ => e

and bind (env, b) =
    case b of
        U.Decl.Datatype (x, n, xncs) => E.pushDatatype env x n xncs
      | U.Decl.RelE (x, t) => E.pushERel env x t NONE
      | U.Decl.NamedE (x, n, t, eo, s) => E.pushENamed env x n t (Option.map (reduceExp env) eo) s

and reduceExp env = U.Exp.mapB {typ = typ, exp = exp, bind = bind} env

fun decl env d = d

val reduce = U.File.mapB {typ = typ, exp = exp, decl = decl, bind = bind} E.empty

end
