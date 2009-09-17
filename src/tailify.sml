(* Copyright (c) 2009, Adam Chlipala
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

structure Tailify :> TAILIFY = struct

open Core

structure U = CoreUtil
structure E = CoreEnv

fun multiLiftExpInExp n e =
    if n = 0 then
        e
    else
        multiLiftExpInExp (n - 1) (E.liftExpInExp 0 e)

structure IS = IntBinarySet
structure IM = IntBinaryMap

type state = {
     cpsed : exp' IM.map,
     rpc : IS.set
}

fun frob file =
    let
        fun exp (e, st : state) =
            case e of
                ENamed n =>
                (case IM.find (#cpsed st, n) of
                     NONE => (e, st)
                   | SOME re => (re, st))
                
              | _ => (e, st)

        and doExp (e, st) = U.Exp.foldMap {kind = fn x => x,
                                           con = fn x => x,
                                           exp = exp} st (ReduceLocal.reduceExp e)

        fun decl (d, st : state) =
            let
                fun makesServerCall b (e, _) =
                    case e of
                        EServerCall _ => true
                      | ETailCall _ => raise Fail "Tailify: ETailCall too early"
                      | ENamed n => IS.member (#rpc st, n)

                      | EPrim _ => false
                      | ERel n => List.nth (b, n)
                      | ECon (_, _, _, NONE) => false
                      | ECon (_, _, _, SOME e) => makesServerCall b e
                      | EFfi _ => false
                      | EFfiApp (_, _, es) => List.exists (makesServerCall b) es
                      | EApp (e1, e2) => makesServerCall b e1 orelse makesServerCall b e2
                      | EAbs (_, _, _, e1) => makesServerCall (false :: b) e1
                      | ECApp (e1, _) => makesServerCall b e1
                      | ECAbs (_, _, e1) => makesServerCall b e1

                      | EKAbs (_, e1) => makesServerCall b e1
                      | EKApp (e1, _) => makesServerCall b e1

                      | ERecord xes => List.exists (fn ((CName s, _), e, _) =>
                                                       not (String.isPrefix "On" s) andalso makesServerCall b e
                                                     | (_, e, _) => makesServerCall b e) xes
                      | EField (e1, _, _) => makesServerCall b e1
                      | EConcat (e1, _, e2, _) => makesServerCall b e1 orelse makesServerCall b e2
                      | ECut (e1, _, _) => makesServerCall b e1
                      | ECutMulti (e1, _, _) => makesServerCall b e1

                      | ECase (e1, pes, _) => makesServerCall b e1
                                              orelse List.exists (fn (p, e) =>
                                                                     makesServerCall (List.tabulate (E.patBindsN p,
                                                                                                  fn _ => false) @ b)
                                                                                     e) pes

                      | EWrite e1 => makesServerCall b e1

                      | EClosure (_, es) => List.exists (makesServerCall b) es

                      | ELet (_, _, e1, e2) => makesServerCall (makesServerCall b e1 :: b) e2

                val makesServerCall = makesServerCall []

                val (d, st) =
                    case #1 d of
                        DValRec vis =>
                        if List.exists (fn (_, _, _, e, _) => makesServerCall e) vis then
                            let
                                val rpc = foldl (fn ((_, n, _, _, _), rpc) =>
                                                    IS.add (rpc, n)) (#rpc st) vis

                                val (cpsed, vis') =
                                    foldl (fn (vi as (x, n, t, e, s), (cpsed, vis')) =>
                                              let
                                                  fun getArgs (t, acc) =
                                                      case #1 t of
                                                          TFun (dom, ran) =>
                                                          getArgs (ran, dom :: acc)
                                                        | _ => (rev acc, t)
                                                  val (ts, ran) = getArgs (t, [])
                                                  val ran = case #1 ran of
                                                                CApp (_, ran) => ran
                                                              | _ => raise Fail "Rpcify: Tail function not transactional"
                                                  val len = length ts

                                                  val loc = #2 e
                                                  val args = ListUtil.mapi
                                                                 (fn (i, _) =>
                                                                     (ERel (len - i - 1), loc))
                                                                 ts
                                                  val k = (EFfi ("Basis", "return"), loc)
                                                  val trans = (CFfi ("Basis", "transaction"), loc)
                                                  val k = (ECApp (k, trans), loc)
                                                  val k = (ECApp (k, ran), loc)
                                                  val k = (EApp (k, (EFfi ("Basis", "transaction_monad"),
                                                                     loc)), loc)
                                                  val re = (ETailCall (n, args, k, ran, ran), loc)
                                                  val (re, _) = foldr (fn (dom, (re, ran)) =>
                                                                          ((EAbs ("x", dom, ran, re),
                                                                            loc),
                                                                           (TFun (dom, ran), loc)))
                                                                      (re, ran) ts

                                                  val be = multiLiftExpInExp (len + 1) e
                                                  val be = ListUtil.foldli
                                                               (fn (i, _, be) =>
                                                                   (EApp (be, (ERel (len - i), loc)), loc))
                                                               be ts
                                                  val ne = (EFfi ("Basis", "bind"), loc)
                                                  val ne = (ECApp (ne, trans), loc)
                                                  val ne = (ECApp (ne, ran), loc)
                                                  val unit = (TRecord (CRecord ((KType, loc), []),
                                                                       loc), loc)
                                                  val ne = (ECApp (ne, unit), loc)
                                                  val ne = (EApp (ne, (EFfi ("Basis", "transaction_monad"),
                                                                       loc)), loc)
                                                  val ne = (EApp (ne, be), loc)
                                                  val ne = (EApp (ne, (ERel 0, loc)), loc)
                                                  val tunit = (CApp (trans, unit), loc)
                                                  val kt = (TFun (ran, tunit), loc)
                                                  val ne = (EAbs ("k", kt, tunit, ne), loc)
                                                  val (ne, res) = foldr (fn (dom, (ne, ran)) =>
                                                                            ((EAbs ("x", dom, ran, ne), loc),
                                                                             (TFun (dom, ran), loc)))
                                                                        (ne, (TFun (kt, tunit), loc)) ts
                                              in
                                                  (IM.insert (cpsed, n, #1 re),
                                                   (x, n, res, ne, s) :: vis')
                                              end)
                                          (#cpsed st, []) vis
                            in
                                ((DValRec (rev vis'), ErrorMsg.dummySpan),
                                 {cpsed = cpsed,
                                  rpc = rpc})
                            end
                        else
                            (d, st)
                      | DVal (x, n, t, e, s) =>
                        (d,
                         {cpsed = #cpsed st,
                          rpc = if makesServerCall e then
                                    IS.add (#rpc st, n)
                                else
                                    #rpc st})
                      | _ => (d, st)
            in
                U.Decl.foldMap {kind = fn x => x,
                                con = fn x => x,
                                exp = exp,
                                decl = fn x => x}
                               st d
            end

        val (file, _) = ListUtil.foldlMap decl
                        {cpsed = IM.empty,
                         rpc = IS.empty}
                        file
    in
        file
    end

end
