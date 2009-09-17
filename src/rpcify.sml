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

structure Rpcify :> RPCIFY = struct

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

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

type state = {
     exported : IS.set,
     export_decls : decl list,

     cpsed : exp' IM.map,
     rpc : IS.set
}

fun frob file =
    let
        val rpcBaseIds = foldl (fn ((d, _), rpcIds) =>
                                   case d of
                                       DVal (_, n, _, (EFfi ("Basis", "rpc"), _), _) => IS.add (rpcIds, n)
                                     | DVal (_, n, _, (ENamed n', _), _) => if IS.member (rpcIds, n') then
                                                                                IS.add (rpcIds, n)
                                                                            else
                                                                                rpcIds
                                     | _ => rpcIds)
                               IS.empty file

        val tfuncs = foldl
                     (fn ((d, _), tfuncs) =>
                         let
                             fun doOne ((x, n, t, e, _), tfuncs) =
                                 let
                                     val loc = #2 e

                                     fun crawl (t, e, args) =
                                         case (#1 t, #1 e) of
                                             (CApp (_, ran), _) =>
                                             SOME (x, rev args, ran, e)
                                           | (TFun (arg, rest), EAbs (x, _, _, e)) =>
                                             crawl (rest, e, (x, arg) :: args)
                                           | (TFun (arg, rest), _) =>
                                             crawl (rest, (EApp (e, (ERel (length args), loc)), loc), ("x", arg) :: args)
                                           | _ => NONE
                                 in
                                     case crawl (t, e, []) of
                                         NONE => tfuncs
                                       | SOME sg => IM.insert (tfuncs, n, sg)
                                 end
                         in
                             case d of
                                 DVal vi => doOne (vi, tfuncs)
                               | DValRec vis => foldl doOne tfuncs vis
                               | _ => tfuncs
                         end)
                     IM.empty file
                             
        fun exp (e, st) =
            let
                fun getApp (e', args) =
                    case e' of
                        ENamed n => SOME (n, args)
                      | EApp (e1, e2) => getApp (#1 e1, e2 :: args)
                      | _ => NONE

                fun newRpc (trans : exp, st : state) =
                    case getApp (#1 trans, []) of
                        NONE => (ErrorMsg.errorAt (#2 trans)
                                                  "RPC code doesn't use a named function or transaction";
                                 (#1 trans, st))
                      | SOME (n, args) =>
                        case IM.find (tfuncs, n) of
                            NONE => ((*Print.prefaces "BAD" [("e", CorePrint.p_exp CoreEnv.empty (e, loc))];*)
                                     raise Fail ("Rpcify: Undetected transaction function " ^ Int.toString n))
                          | SOME (_, _, ran, _) =>
                            let
                                val loc = #2 trans

                                val (exported, export_decls) =
                                    if IS.member (#exported st, n) then
                                        (#exported st, #export_decls st)
                                    else
                                        (IS.add (#exported st, n),
                                         (DExport (Rpc ReadWrite, n), loc) :: #export_decls st)

                                val st = {exported = exported,
                                          export_decls = export_decls,
                                          cpsed = #cpsed st,
                                          rpc = #rpc st}

                                val k = (ECApp ((EFfi ("Basis", "return"), loc),
                                                (CFfi ("Basis", "transaction"), loc)), loc)
                                val k = (ECApp (k, ran), loc)
                                val k = (EApp (k, (EFfi ("Basis", "transaction_monad"), loc)), loc)
                                val e' = EServerCall (n, args, k, ran, ran)
                            in
                                (e', st)
                            end
            in
                case e of
                    EApp ((ECApp ((EFfi ("Basis", "rpc"), _), ran), _), trans) => newRpc (trans, st)
                  | EApp ((ECApp ((ENamed n, _), ran), _), trans) =>
                    if IS.member (rpcBaseIds, n) then
                        newRpc (trans, st)
                    else
                        (e, st)

                  | ENamed n =>
                    (case IM.find (#cpsed st, n) of
                         NONE => (e, st)
                       | SOME re => (re, st))

                  | _ => (e, st)
            end

        and doExp (e, st) = U.Exp.foldMap {kind = fn x => x,
                                           con = fn x => x,
                                           exp = exp} st (ReduceLocal.reduceExp e)

        fun decl (d, st : state) =
            let
                val makesServerCall = U.Exp.exists {kind = fn _ => false,
                                                    con = fn _ => false,
                                                    exp = fn EFfi ("Basis", "rpc") => true
                                                           | ENamed n => IS.member (#rpc st, n)
                                                           | _ => false}

                val (d, st) =
                    case #1 d of
                        DValRec vis =>
                        if List.exists (fn (_, _, _, e, _) => makesServerCall e) vis then
                            let
                                val all = foldl (fn ((_, n, _, _, _), all) => IS.add (all, n)) IS.empty vis

                                val usesRec = U.Exp.exists {kind = fn _ => false,
                                                            con = fn _ => false,
                                                            exp = fn ENamed n => IS.member (all, n)
                                                                   | _ => false}

                                val noRec = not o usesRec

                                fun tailOnly (e, _) =
                                    case e of
                                        EPrim _ => true
                                      | ERel _ => true
                                      | ENamed _ => true
                                      | ECon (_, _, _, SOME e) => noRec e
                                      | ECon _ => true
                                      | EFfi _ => true
                                      | EFfiApp (_, _, es) => List.all noRec es
                                      | EApp (e1, e2) => noRec e2 andalso tailOnly e1
                                      | EAbs (_, _, _, e) => noRec e
                                      | ECApp (e1, _) => tailOnly e1
                                      | ECAbs (_, _, e) => noRec e

                                      | EKAbs (_, e) => noRec e
                                      | EKApp (e1, _) => tailOnly e1

                                      | ERecord xes => List.all (noRec o #2) xes
                                      | EField (e1, _, _) => noRec e1
                                      | EConcat (e1, _, e2, _) => noRec e1 andalso noRec e2
                                      | ECut (e1, _, _) => noRec e1
                                      | ECutMulti (e1, _, _) => noRec e1

                                      | ECase (e1, pes, _) => noRec e1 andalso List.all (tailOnly o #2) pes

                                      | EWrite e1 => noRec e1

                                      | EClosure (_, es) => List.all noRec es

                                      | ELet (_, _, e1, e2) => noRec e1 andalso tailOnly e2

                                      | EServerCall (_, es, (EAbs (_, _, _, e), _), _, _) =>
                                        List.all noRec es andalso tailOnly e
                                      | EServerCall (_, es, e, _, _) => List.all noRec es andalso noRec e

                                      | ETailCall _ => raise Fail "Rpcify: ETailCall too early"

                                fun tailOnlyF e =
                                    case #1 e of
                                        EAbs (_, _, _, e) => tailOnlyF e
                                      | ECAbs (_, _, e) => tailOnlyF e
                                      | EKAbs (_, e) => tailOnlyF e
                                      | _ => tailOnly e

                                val nonTail = foldl (fn ((_, n, _, e, _), nonTail) =>
                                                        if tailOnlyF e then
                                                            nonTail
                                                        else
                                                            IS.add (nonTail, n)) IS.empty vis
                            in
                                if IS.isEmpty nonTail then
                                    (d, {exported = #exported st,
                                         export_decls = #export_decls st,
                                         cpsed = #cpsed st,
                                         rpc = IS.union (#rpc st, all)})
                                else
                                    let
                                        val rpc = foldl (fn ((_, n, _, _, _), rpc) =>
                                                            IS.add (rpc, n)) (#rpc st) vis

                                        val (cpsed, vis') =
                                            foldl (fn (vi as (x, n, t, e, s), (cpsed, vis')) =>
                                                      if IS.member (nonTail, n) then
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
                                                          end
                                                      else
                                                          (cpsed, vi :: vis'))
                                                  (#cpsed st, []) vis
                                    in
                                        ((DValRec (rev vis'), ErrorMsg.dummySpan),
                                         {exported = #exported st,
                                          export_decls = #export_decls st,
                                          cpsed = cpsed,
                                          rpc = rpc})
                                    end
                            end
                        else
                            (d, st)
                      | DVal (x, n, t, e, s) =>
                        (d,
                         {exported = #exported st,
                          export_decls = #export_decls st,
                          cpsed = #cpsed st,
                          rpc = if makesServerCall e then
                                    IS.add (#rpc st, n)
                                else
                                    #rpc st})
                      | _ => (d, st)

                val (d, st) = U.Decl.foldMap {kind = fn x => x,
                                              con = fn x => x,
                                              exp = exp,
                                              decl = fn x => x}
                              st d
            in
                (#export_decls st @ [d],
                 {exported = #exported st,
                  export_decls = [],
                  cpsed = #cpsed st,
                  rpc = #rpc st})
            end

        val (file, _) = ListUtil.foldlMapConcat decl
                        {exported = IS.empty,
                         export_decls = [],
                         cpsed = IM.empty,
                         rpc = rpcBaseIds}
                        file
    in
        file
    end

end
