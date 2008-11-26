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
                              | _ => raise Fail "Reduce.con: CRel [1]"
                        else
                            case env of
                                UnknownC :: rest => find (n' - 1, rest, lift + 1)
                              | KnownC _ :: rest => find (n' - 1, rest, lift)
                              | UnknownE :: rest => find (n' - 1, rest, lift)
                              | KnownE _ :: rest => find (n' - 1, rest, lift)
                              | Lift (lift', _) :: rest => find (n' - 1, rest, lift + lift')
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

        fun exp env e = e
    in
        {con = con, exp = exp}
    end

fun con namedC c = #con (conAndExp (namedC, IM.empty)) [] c
fun exp (namedC, namedE) e = #exp (conAndExp (namedC, namedE)) [] e

fun reduce file =
    let
        fun doDecl (d as (_, loc), st as (namedC, namedE)) =
            case #1 d of
                DCon (x, n, k, c) =>
                let
                    val c = con namedC c
                in
                    ((DCon (x, n, k, c), loc),
                     (IM.insert (namedC, n, c), namedE))
                end
              | DDatatype (x, n, ps, cs) =>
                ((DDatatype (x, n, ps, map (fn (x, n, co) => (x, n, Option.map (con namedC) co)) cs), loc),
                 st)
              | DVal (x, n, t, e, s) =>
                let
                    val t = con namedC t
                    val e = exp (namedC, namedE) e
                in
                    ((DVal (x, n, t, e, s), loc),
                     (namedC, IM.insert (namedE, n, e)))
                end
              | DValRec vis =>
                ((DValRec (map (fn (x, n, t, e, s) => (x, n, con namedC t, exp (namedC, namedE) e, s)) vis), loc),
                 st)
              | DExport _ => (d, st)
              | DTable (s, n, c, s') => ((DTable (s, n, con namedC c, s'), loc), st)
              | DSequence _ => (d, st)
              | DDatabase _ => (d, st)
              | DCookie (s, n, c, s') => ((DCookie (s, n, con namedC c, s'), loc), st)

        val (file, _) = ListUtil.foldlMap doDecl (IM.empty, IM.empty) file
    in
        file
    end

end
