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

structure IM = IntBinaryMap

datatype env_item =
         UnknownC
       | KnownC of con

       | UnknownE
       | KnownE of exp

       | Lift of int * int

type env = env_item list

fun conAndExp (namedC, namedE) =
    let
        fun con env (all as (c, loc)) =
            case c of
                TFun (c1, c2) => (TFun (con env c1, con env c2), loc)
              | TCFun (x, k, c2) => (TCFun (x, k, con (UnknownC :: env) c2), loc)
              | TRecord c => (TRecord (con env c), loc)

              | CRel n =>
                let
                    fun find (n', env, lift) =
                        if n' = 0 then
                            case env of
                                UnknownC :: _ => (CRel (n + lift), loc)
                              | KnownC c :: _ => con (Lift (lift, 0) :: env) c
                              | Lift (lift', _) :: rest => find (0, rest, lift + lift')
                              | _ => raise Fail "Reduce.con: CRel [1]"
                        else
                            case env of
                                UnknownC :: rest => find (n' - 1, rest, lift + 1)
                              | KnownC _ :: rest => find (n' - 1, rest, lift)
                              | UnknownE :: rest => find (n' - 1, rest, lift)
                              | KnownE _ :: rest => find (n' - 1, rest, lift)
                              | Lift (lift', _) :: rest => find (n', rest, lift + lift')
                              | [] => raise Fail "Reduce.con: CRel [2]"
                in
                    find (n, env, 0)
                end
              | CNamed n =>
                (case IM.find (namedC, n) of
                     NONE => all
                   | SOME c => c)
              | CFfi _ => all
              | CApp (c1, c2) =>
                let
                    val c1 = con env c1
                    val c2 = con env c2
                in
                    case #1 c1 of
                        CAbs (_, _, b) =>
                        con (KnownC c2 :: env) b

                      | CApp ((CApp (fold as (CFold _, _), f), _), i) =>
                        (case #1 c2 of
                             CRecord (_, []) => i
                           | CRecord (k, (x, c) :: rest) =>
                             con env (CApp ((CApp ((CApp (f, x), loc), c), loc),
                                            (CApp ((CApp ((CApp (fold, f), loc), i), loc),
                                                   (CRecord (k, rest), loc)), loc)), loc)
                           | _ => (CApp (c1, c2), loc))                           

                      | _ => (CApp (c1, c2), loc)
                end
              | CAbs (x, k, b) => (CAbs (x, k, con (UnknownC :: env) b), loc)

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
                      | _ => (CConcat (c1, c2), loc)
                end
              | CFold _ => all

              | CUnit => all

              | CTuple cs => (CTuple (map (con env) cs), loc)
              | CProj (c, n) =>
                let
                    val c = con env c
                in
                    case #1 c of
                        CTuple cs => List.nth (cs, n - 1)
                      | _ => (CProj (c, n), loc)
                end

        fun patCon pc =
            case pc of
                PConVar _ => pc
              | PConFfi {mod = m, datatyp, params, con = c, arg, kind} =>
                PConFfi {mod = m, datatyp = datatyp, params = params, con = c,
                         arg = Option.map (con (map (fn _ => UnknownC) params)) arg,
                         kind = kind}


        val k = (KType, ErrorMsg.dummySpan)
        fun doPart e (this as (x, t), rest) =
            ((x, (EField (e, x, {field = t, rest = (CRecord (k, rest), #2 t)}), #2 t), t),
             this :: rest)

        fun exp env (all as (e, loc)) =
            case e of
                EPrim _ => all
              | ERel n =>
                let
                    fun find (n', env, liftC, liftE) =
                        if n' = 0 then
                            case env of
                                UnknownE :: _ => (ERel (n + liftE), loc)
                              | KnownE e :: _ => exp (Lift (liftC, liftE) :: env) e
                              | Lift (liftC', liftE') :: rest => find (0, rest, liftC + liftC', liftE + liftE')
                              | _ => raise Fail "Reduce.exp: ERel [1]"
                        else
                            case env of
                                UnknownC :: rest => find (n' - 1, rest, liftC + 1, liftE)
                              | KnownC _ :: rest => find (n' - 1, rest, liftC, liftE)
                              | UnknownE :: rest => find (n' - 1, rest, liftC, liftE + 1)
                              | KnownE _ :: rest => find (n' - 1, rest, liftC, liftE)
                              | Lift (liftC', liftE') :: rest => find (n', rest, liftC + liftC', liftE + liftE')
                              | [] => raise Fail "Reduce.exp: ERel [2]"
                in
                    find (n, env, 0, 0)
                end
              | ENamed n =>
                (case IM.find (namedE, n) of
                     NONE => all
                   | SOME e => e)
              | ECon (dk, pc, cs, eo) => (ECon (dk, patCon pc,
                                                map (con env) cs, Option.map (exp env) eo), loc)
              | EFfi _ => all
              | EFfiApp (m, f, es) => (EFfiApp (m, f, map (exp env) es), loc)

              | EApp (e1, e2) =>
                let
                    val e1 = exp env e1
                    val e2 = exp env e2
                in
                    case #1 e1 of
                        EAbs (_, _, _, b) => exp (KnownE e2 :: env) b
                      | _ => (EApp (e1, e2), loc)
                end

              | EAbs (x, dom, ran, e) => (EAbs (x, con env dom, con env ran, exp (UnknownE :: env) e), loc)

              | ECApp (e, c) =>
                let
                    val e = exp env e
                    val c = con env c
                in
                    case #1 e of
                        ECAbs (_, _, b) => exp (KnownC c :: env) b
                      | _ => (ECApp (e, c), loc)
                end

              | ECAbs (x, k, e) => (ECAbs (x, k, exp (UnknownC :: env) e), loc)

              | ERecord xcs => (ERecord (map (fn (x, e, t) => (con env x, exp env e, con env t)) xcs), loc)
              | EField (e, c, {field, rest}) =>
                let
                    val e = exp env e
                    val c = con env c

                    fun default () = (EField (e, c, {field = con env field, rest = con env rest}), loc)
                in
                    case (#1 e, #1 c) of
                        (ERecord xcs, CName x) =>
                        (case List.find (fn ((CName x', _), _, _) => x' = x | _ => false) xcs of
                             NONE => default ()
                           | SOME (_, e, _) => e)
                      | _ => default ()
                end

              | EConcat (e1, c1, e2, c2) =>
                let
                    val e1 = exp env e1
                    val e2 = exp env e2
                in
                    case (#1 e1, #1 e2) of
                        (ERecord xes1, ERecord xes2) => (ERecord (xes1 @ xes2), loc)
                      | _ =>
                        let
                            val c1 = con env c1
                            val c2 = con env c2
                        in
                            case (#1 c1, #1 c2) of
                                (CRecord (k, xcs1), CRecord (_, xcs2)) =>
                                let
                                    val (xes1, rest) = ListUtil.foldlMap (doPart e1) [] xcs1
                                    val (xes2, _) = ListUtil.foldlMap (doPart e2) rest xcs2
                                in
                                    exp env (ERecord (xes1 @ xes2), loc)
                                end
                              | _ => (EConcat (e1, c1, e2, c2), loc)
                        end
                end

              | ECut (e, c, {field, rest}) =>
                let
                    val e = exp env e
                    val c = con env c

                    fun default () =
                        let
                            val rest = con env rest
                        in
                            case #1 rest of
                                CRecord (k, xcs) =>
                                let
                                    val (xes, _) = ListUtil.foldlMap (doPart e) [] xcs
                                in
                                    exp env (ERecord xes, loc)
                                end
                              | _ => (ECut (e, c, {field = con env field, rest = rest}), loc)
                        end
                in
                    case (#1 e, #1 c) of
                        (ERecord xes, CName x) =>
                        if List.all (fn ((CName _, _), _, _) => true | _ => false) xes then
                            (ERecord (List.filter (fn ((CName x', _), _, _) => x' <> x
                                                    | _ => raise Fail "Reduce: ECut") xes), loc)
                        else
                            default ()
                      | _ => default ()
                end

              | ECutMulti (e, c, {rest}) =>
                let
                    val e = exp env e
                    val c = con env c

                    fun default () =
                        let
                            val rest = con env rest
                        in
                            case #1 rest of
                                CRecord (k, xcs) =>
                                let
                                    val (xes, _) = ListUtil.foldlMap (doPart e) [] xcs
                                in
                                    exp env (ERecord xes, loc)
                                end
                              | _ => (ECutMulti (e, c, {rest = rest}), loc)
                        end
                in
                    case (#1 e, #1 c) of
                        (ERecord xes, CRecord (_, xcs)) =>
                        if List.all (fn ((CName _, _), _, _) => true | _ => false) xes
                           andalso List.all (fn ((CName _, _), _) => true | _ => false) xcs then
                            (ERecord (List.filter (fn ((CName x', _), _, _) =>
                                                      List.all (fn ((CName x, _), _) => x' <> x
                                                                 | _ => raise Fail "Reduce: ECutMulti [1]") xcs
                                                    | _ => raise Fail "Reduce: ECutMulti [2]") xes), loc)
                        else
                            default ()
                      | _ => default ()
                end

              | EFold _ => all

              | ECase (e, pes, {disc, result}) =>
                let
                    fun patBinds (p, _) =
                        case p of
                            PWild => 0
                          | PVar _ => 1
                          | PPrim _ => 0
                          | PCon (_, _, _, NONE) => 0
                          | PCon (_, _, _, SOME p) => patBinds p
                          | PRecord xpts => foldl (fn ((_, p, _), n) => n + patBinds p) 0 xpts

                    fun pat (all as (p, loc)) =
                        case p of
                            PWild => all
                          | PVar (x, t) => (PVar (x, con env t), loc)
                          | PPrim _ => all
                          | PCon (dk, pc, cs, po) =>
                            (PCon (dk, patCon pc, map (con env) cs, Option.map pat po), loc)
                          | PRecord xpts => (PRecord (map (fn (x, p, t) => (x, pat p, con env t)) xpts), loc)
                in
                    (ECase (exp env e,
                            map (fn (p, e) => (pat p,
                                               exp (List.tabulate (patBinds p, fn _ => UnknownE) @ env) e))
                                pes, {disc = con env disc, result = con env result}), loc)
                end

              | EWrite e => (EWrite (exp env e), loc)
              | EClosure (n, es) => (EClosure (n, map (exp env) es), loc)

              | ELet (x, t, e1, e2) => (ELet (x, con env t, exp env e1, exp (UnknownE :: env) e2), loc)
    in
        {con = con, exp = exp}
    end

fun con namedC env c = #con (conAndExp (namedC, IM.empty)) env c
fun exp (namedC, namedE) env e = #exp (conAndExp (namedC, namedE)) env e

fun reduce file =
    let
        fun doDecl (d as (_, loc), st as (namedC, namedE)) =
            case #1 d of
                DCon (x, n, k, c) =>
                let
                    val c = con namedC [] c
                in
                    ((DCon (x, n, k, c), loc),
                     (IM.insert (namedC, n, c), namedE))
                end
              | DDatatype (x, n, ps, cs) =>
                let
                    val env = map (fn _ => UnknownC) ps
                in
                    ((DDatatype (x, n, ps, map (fn (x, n, co) => (x, n, Option.map (con namedC env) co)) cs), loc),
                     st)
                end
              | DVal (x, n, t, e, s) =>
                let
                    val t = con namedC [] t
                    val e = exp (namedC, namedE) [] e
                in
                    ((DVal (x, n, t, e, s), loc),
                     (namedC, IM.insert (namedE, n, e)))
                end
              | DValRec vis =>
                ((DValRec (map (fn (x, n, t, e, s) => (x, n, con namedC [] t, exp (namedC, namedE) [] e, s)) vis), loc),
                 st)
              | DExport _ => (d, st)
              | DTable (s, n, c, s') => ((DTable (s, n, con namedC [] c, s'), loc), st)
              | DSequence _ => (d, st)
              | DDatabase _ => (d, st)
              | DCookie (s, n, c, s') => ((DCookie (s, n, con namedC [] c, s'), loc), st)

        val (file, _) = ListUtil.foldlMap doDecl (IM.empty, IM.empty) file
    in
        file
    end

end
