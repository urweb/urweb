(* Copyright (c) 2008-2010, 2013, Adam Chlipala
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

fun multiLiftExpInExp n e =
    if n = 0 then
        e
    else
        multiLiftExpInExp (n - 1) (CoreEnv.liftExpInExp 0 e)

datatype env_item =
         Unknown
       | Known of exp

       | UnknownC
       | KnownC of con

       | Lift of int * int

type env = env_item list

val deKnown = List.filter (fn Known _ => false
                            | KnownC _ => false
                            | _ => true)

fun p_env_item ei =
    Print.PD.string (case ei of
                         Unknown => "?"
                       | Known _ => "K"
                       | UnknownC => "C?"
                       | KnownC _ => "CK"
                       | Lift _ => "^")
                          
datatype result = Yes of env | No | Maybe

fun match (env, p : pat, e : exp) =
    let
        val baseline = length env

        fun match (env, p, e) =
            case (#1 p, #1 e) of
                (PVar (x, t), _) => Yes (Known (multiLiftExpInExp (length env - baseline) e) :: env)

              | (PPrim p, EPrim p') =>
                if Prim.equal (p, p') then
                    Yes env
                else
                    No

              | (PCon (_, PConVar n1, _, NONE), ECon (_, PConVar n2, _, NONE)) =>
                if n1 = n2 then
                    Yes env
                else
                    No

              | (PCon (_, PConVar n1, _, SOME p), ECon (_, PConVar n2, _, SOME e)) =>
                if n1 = n2 then
                    match (env, p, e)
                else
                    No

              | (PCon (_, PConFfi {mod = m1, con = con1, ...}, _, NONE),
                 ECon (_, PConFfi {mod = m2, con = con2, ...}, _, NONE)) =>
                if m1 = m2 andalso con1 = con2 then
                    Yes env
                else
                    No

              | (PCon (_, PConFfi {mod = m1, con = con1, ...}, _, SOME ep),
                 ECon (_, PConFfi {mod = m2, con = con2, ...}, _, SOME e)) =>
                if m1 = m2 andalso con1 = con2 then
                    match (env, p, e)
                else
                    No

              | (PRecord xps, ERecord xes) =>
                if List.exists (fn ((CName _, _), _, _) => false
                                 | _ => true) xes then
                    Maybe
                else
                    let
                        fun consider (xps, env) =
                            case xps of
                                [] => Yes env
                              | (x, p, _) :: rest =>
                                case List.find (fn ((CName x', _), _, _) => x' = x
                                                 | _ => false) xes of
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
    in
        match (env, p, e)
    end

fun con env (all as (c, loc)) =
    ((*Print.prefaces "con" [("c", CorePrint.p_con CoreEnv.empty all),
                           ("env", Print.p_list p_env_item env)];*)
     case c of
         TFun (c1, c2) => (TFun (con env c1, con env c2), loc)
       | TCFun (x, k, c2) => (TCFun (x, k, con (UnknownC :: env) c2), loc)
       | TKFun (x, c2) => (TKFun (x, con env c2), loc)
       | TRecord c => (TRecord (con env c), loc)

       | CRel n =>
         let
             fun find (n', env, nudge, liftC) =
                 case env of
                     [] => raise Fail "ReduceLocal.con: CRel"
                   | Unknown :: rest => find (n', rest, nudge, liftC)
                   | Known _ :: rest => find (n', rest, nudge, liftC)
                   | Lift (liftC', _) :: rest => find (n', rest, nudge + liftC',
                                                       liftC + liftC')
                   | UnknownC :: rest =>
                     if n' = 0 then
                         (CRel (n + nudge), loc)
                     else
                         find (n' - 1, rest, nudge, liftC + 1)
                   | KnownC c :: rest =>
                     if n' = 0 then
                         con (Lift (liftC, 0) :: rest) c
                     else
                         find (n' - 1, rest, nudge - 1, liftC)
         in
             (*print (Int.toString n ^ ": " ^ e2s env ^ "\n");*)
             find (n, env, 0, 0)
         end
       | CNamed _ => all
       | CFfi _ => all
       | CApp (c1, c2) =>
         let
             val c1 = con env c1
             val c2 = con env c2
         in
             case #1 c1 of
                 CAbs (_, _, b) =>
                 con (KnownC c2 :: deKnown env) b

               | CApp ((CMap (dom, ran), _), f) =>
                 (case #1 c2 of
                      CRecord (_, []) => (CRecord (ran, []), loc)
                    | CRecord (_, (x, c) :: rest) =>
                      con (deKnown env)
                          (CConcat ((CRecord (ran, [(x, (CApp (f, c), loc))]), loc),
                                    (CApp (c1, (CRecord (dom, rest), loc)), loc)), loc)
                    | _ => (CApp (c1, c2), loc))                           

               | _ => (CApp (c1, c2), loc)
         end
       | CAbs (x, k, b) => (CAbs (x, k, con (UnknownC :: env) b), loc)

       | CKApp (c1, k) =>
         let
             val c1 = con env c1
         in
             case #1 c1 of
                 CKAbs (_, b) =>
                 con (deKnown env) b

               | _ => (CKApp (c1, k), loc)
         end
       | CKAbs (x, b) => (CKAbs (x, con env b), loc)

       | CName _ => all

       | CRecord (k, xcs) => (CRecord (k, map (fn (x, c) => (con env x, con env c)) xcs), loc)
       | CConcat (c1, c2) =>
         let
             val c1 = con env c1
             val c2 = con env c2
         in
             case (#1 c1, #1 c2) of
                 (CRecord (k, xcs1), CRecord (_, xcs2)) =>
                 (CRecord (k, xcs1 @ xcs2), loc)
               | (CRecord (_, []), _) => c2
               | (_, CRecord (_, [])) => c1
               | _ => (CConcat (c1, c2), loc)
         end
       | CMap _ => all

       | CUnit => all

       | CTuple cs => (CTuple (map (con env) cs), loc)
       | CProj (c, n) =>
         let
             val c = con env c
         in
             case #1 c of
                 CTuple cs => List.nth (cs, n - 1)
               | _ => (CProj (c, n), loc)
         end)

fun patCon pc =
    case pc of
        PConVar _ => pc
      | PConFfi {mod = m, datatyp, params, con = c, arg, kind} =>
        PConFfi {mod = m, datatyp = datatyp, params = params, con = c,
                 arg = Option.map (con (map (fn _ => UnknownC) params)) arg,
                 kind = kind}

fun exp env (all as (e, loc)) =
    ((*Print.prefaces "exp" [("e", CorePrint.p_exp CoreEnv.empty all)];*)
     case e of
         EPrim _ => all
       | ERel n =>
         let
             fun find (n', env, nudge, liftC, liftE) =
                 case env of
                     [] => (ERel (n + nudge), loc)
                   | Lift (liftC', liftE') :: rest => find (n', rest, nudge + liftE', liftC + liftC', liftE + liftE')
                   | UnknownC :: rest => find (n', rest, nudge, liftC + 1, liftE)
                   | KnownC _ :: rest => find (n', rest, nudge, liftC, liftE)
                   | Unknown :: rest =>
                     if n' = 0 then
                         (ERel (n + nudge), loc)
                     else
                         find (n' - 1, rest, nudge, liftC, liftE + 1)
                   | Known e :: rest =>
                     if n' = 0 then
                         ((*print "SUBSTITUTING\n";*)
                           exp (Lift (liftC, liftE) :: rest) e)
                     else
                         find (n' - 1, rest, nudge - 1, liftC, liftE)
         in
             find (n, env, 0, 0, 0)
         end
       | ENamed _ => all
       | ECon (dk, pc, cs, eo) => (ECon (dk, patCon pc, map (con env) cs, Option.map (exp env) eo), loc)
       | EFfi _ => all
       | EFfiApp (m, f, es) => (EFfiApp (m, f, map (fn (e, t) => (exp env e, con env t)) es), loc)

       | EApp (e1, e2) =>
         let
             val e1 = exp env e1
             val e2 = exp env e2
         in
             case #1 e1 of
                 EAbs (_, _, _, b) => exp (Known e2 :: deKnown env) b
               | _ => (EApp (e1, e2), loc)
         end

       | EAbs (x, dom, ran, e) => (EAbs (x, con env dom, con env ran, exp (Unknown :: env) e), loc)

       | ECApp (e, c) =>
         let
             val e = exp env e
             val c = con env c
         in
             case #1 e of
                 ECAbs (_, _, b) => exp (KnownC c :: deKnown env) b
               | _ => (ECApp (e, c), loc)
         end

       | ECAbs (x, k, e) => (ECAbs (x, k, exp (UnknownC :: env) e), loc)

       | EKApp (e, k) => (EKApp (exp env e, k), loc)
       | EKAbs (x, e) => (EKAbs (x, exp env e), loc)

       | ERecord xcs => (ERecord (map (fn (x, e, t) => (con env x, exp env e, con env t)) xcs), loc)
       | EField (e, c, {field = f, rest = r}) =>
         let
             val e = exp env e
             val c = con env c

             fun default () = (EField (e, c, {field = con env f, rest = con env r}), loc)
         in
             case (#1 e, #1 c) of
                 (ERecord xcs, CName x) =>
                 (case List.find (fn ((CName x', _), _, _) => x' = x | _ => false) xcs of
                      NONE => default ()
                    | SOME (_, e, _) => e)
               | _ => default ()
         end

       | EConcat (e1, c1, e2, c2) => (EConcat (exp env e1, con env c1, exp env e2, con env c2), loc)
       | ECut (e, c, {field = f, rest = r}) => (ECut (exp env e,
                                                      con env c,
                                                      {field = con env f, rest = con env r}), loc)
       | ECutMulti (e, c, {rest = r}) => (ECutMulti (exp env e, con env c, {rest = con env r}), loc)

       | ECase (e, pes, {disc = d, result = r}) =>
         let
             val others = {disc = con env d, result = con env r}

             fun patBinds (p, _) =
                 case p of
                     PVar _ => 1
                   | PPrim _ => 0
                   | PCon (_, _, _, NONE) => 0
                   | PCon (_, _, _, SOME p) => patBinds p
                   | PRecord xpts => foldl (fn ((_, p, _), n) => n + patBinds p) 0 xpts

             fun pat (all as (p, loc)) =
                 case p of
                     PVar (x, t) => (PVar (x, con env t), loc)
                   | PPrim _ => all
                   | PCon (dk, pc, cs, po) =>
                     (PCon (dk, patCon pc, map (con env) cs, Option.map pat po), loc)
                   | PRecord xpts => (PRecord (map (fn (x, p, t) => (x, pat p, con env t)) xpts), loc)

             fun push () =
                 (ECase (exp env e,
                         map (fn (p, e) => (pat p,
                                            exp (List.tabulate (patBinds p,
                                                                fn _ => Unknown) @ env) e))
                             pes, others), loc)

             fun search pes =
                 case pes of
                     [] => push ()
                   | (p, body) :: pes =>
                     case match (env, p, e) of
                         No => search pes
                       | Maybe => push ()
                       | Yes env' => exp env' body
         in
             search pes
         end

       | EWrite e => (EWrite (exp env e), loc)
       | EClosure (n, es) => (EClosure (n, map (exp env) es), loc)

       | ELet (x, t, e1, e2) => (ELet (x, con env t, exp env e1, exp (Unknown :: env) e2), loc)

       | EServerCall (n, es, t, fm) => (EServerCall (n, map (exp env) es, con env t, fm), loc))

fun reduce file =
    let
        fun doDecl (d as (_, loc)) =
            ((*Print.prefaces "decl" [("d", CorePrint.p_decl CoreEnv.empty d)];*)
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
               | DView _ => d
               | DIndex _ => d
               | DDatabase _ => d
               | DCookie _ => d
               | DStyle _ => d
               | DTask (e1, e2) => (DTask (exp [] e1, exp [] e2), loc)
               | DPolicy e1 => (DPolicy (exp [] e1), loc)
               | DOnError _ => d)
    in
        map doDecl file
    end

val reduceExp = exp []
val reduceCon = con []

end
