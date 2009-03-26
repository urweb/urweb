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

structure IS = IntBinarySet
structure IM = IntBinaryMap

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

fun multiLiftExpInExp n e =
    if n = 0 then
        e
    else
        multiLiftExpInExp (n - 1) (E.liftExpInExp 0 e)

val ssBasis = SS.addList (SS.empty,
                          ["requestHeader",
                           "query",
                           "dml",
                           "nextval",
                           "channel",
                           "subscribe",
                           "send"])

val csBasis = SS.addList (SS.empty,
                          ["get",
                           "set",
                           "alert",
                           "recv"])

type state = {
     cpsed : int IM.map,
     cpsed_range : con IM.map,
     cps_decls : (string * int * con * exp * string) list,

     exported : IS.set,
     export_decls : decl list,

     maxName : int
}

fun frob file =
    let
        fun sideish (basis, ssids) e =
            U.Exp.exists {kind = fn _ => false,
                          con = fn _ => false,
                          exp = fn ENamed n => IS.member (ssids, n)
                                 | EFfi ("Basis", x) => SS.member (basis, x)
                                 | EFfiApp ("Basis", x, _) => SS.member (basis, x)
                                 | _ => false}
                         (U.Exp.map {kind = fn x => x,
                                     con = fn x => x,
                                     exp = fn ERecord _ => ERecord []
                                            | x => x} e)

        fun whichIds basis =
            let
                fun decl ((d, _), ssids) =
                    let
                        val impure = sideish (basis, ssids)
                    in
                        case d of
                            DVal (_, n, _, e, _) => if impure e then
                                                        IS.add (ssids, n)
                                                    else
                                                        ssids
                          | DValRec xes => if List.exists (fn (_, _, _, e, _) => impure e) xes then
                                               foldl (fn ((_, n, _, _, _), ssids) => IS.add (ssids, n))
                                                     ssids xes
                                           else
                                               ssids
                          | _ => ssids
                    end
            in
                foldl decl IS.empty file
            end

        val ssids = whichIds ssBasis
        val csids = whichIds csBasis

        fun sideish' (basis, ids) extra =
            sideish (basis, IM.foldli (fn (id, _, ids) => IS.add (ids, id)) ids extra)

        val serverSide = sideish' (ssBasis, ssids)
        val clientSide = sideish' (csBasis, csids)

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
                    let
                        val loc = #2 e'
                    in
                        case #1 e' of
                            ENamed n => (n, args)
                          | EApp (e1, e2) => getApp (e1, e2 :: args)
                          | _ => (ErrorMsg.errorAt loc "Mixed client/server code doesn't use a named function for server part";
                                  (*Print.prefaces "Bad" [("e", CorePrint.p_exp CoreEnv.empty (e, ErrorMsg.dummySpan))];*)
                                  (0, []))
                    end

                fun newRpc (trans1, trans2, st : state) =
                    let
                        val loc = #2 trans1

                        val (n, args) = getApp (trans1, [])

                        val (exported, export_decls) =
                            if IS.member (#exported st, n) then
                                (#exported st, #export_decls st)
                            else
                                (IS.add (#exported st, n),
                                 (DExport (Rpc, n), loc) :: #export_decls st)

                        val st = {cpsed = #cpsed st,
                                  cpsed_range = #cpsed_range st,
                                  cps_decls = #cps_decls st,

                                  exported = exported,
                                  export_decls = export_decls,

                                  maxName = #maxName st}

                        val ran =
                            case IM.find (tfuncs, n) of
                                NONE => ((*Print.prefaces "BAD" [("e", CorePrint.p_exp CoreEnv.empty (e, loc))];*)
                                         raise Fail ("Rpcify: Undetected transaction function " ^ Int.toString n))
                              | SOME (_, _, ran, _) => ran

                        val e' = EServerCall (n, args, trans2, ran)
                    in
                        (e', st)
                    end

                fun newCps (t1, t2, trans1, trans2, st) =
                    let
                        val loc = #2 trans1

                        val (n, args) = getApp (trans1, [])

                        fun makeCall n' =
                            let
                                val e = (ENamed n', loc)
                                val e = (EApp (e, trans2), loc)
                            in
                                #1 (foldl (fn (arg, e) => (EApp (e, arg), loc)) e args)
                            end
                    in
                        case IM.find (#cpsed_range st, n) of
                            SOME kdom =>
                            (case args of
                                 [] => raise Fail "Rpcify: cps'd function lacks first argument"
                               | ke :: args =>
                                 let
                                     val ke' = (EFfi ("Basis", "bind"), loc)
                                     val ke' = (ECApp (ke', (CFfi ("Basis", "transaction"), loc)), loc)
                                     val ke' = (ECApp (ke', kdom), loc)
                                     val ke' = (ECApp (ke', t2), loc)
                                     val ke' = (EApp (ke', (EFfi ("Basis", "transaction_monad"), loc)), loc)
                                     val ke' = (EApp (ke', (EApp (E.liftExpInExp 0 ke, (ERel 0, loc)), loc)), loc)
                                     val ke' = (EApp (ke', E.liftExpInExp 0 trans2), loc)
                                     val ke' = (EAbs ("x", kdom,
                                                      (CApp ((CFfi ("Basis", "transaction"), loc), t2), loc),
                                                      ke'), loc)

                                     val e' = (ENamed n, loc)
                                     val e' = (EApp (e', ke'), loc)
                                     val e' = foldl (fn (arg, e') => (EApp (e', arg), loc)) e' args
                                     val (e', st) = doExp (e', st)
                                 in
                                     (#1 e', st)
                                 end)
                          | NONE =>
                            case IM.find (#cpsed st, n) of
                                SOME n' => (makeCall n', st)
                              | NONE =>
                                let
                                    val (name, fargs, ran, e) =
                                        case IM.find (tfuncs, n) of
                                            NONE => (Print.prefaces "BAD" [("e",
                                                                            CorePrint.p_exp CoreEnv.empty (e, loc))];
                                                     raise Fail "Rpcify: Undetected transaction function [2]")
                                          | SOME x => x
                                                      
                                    val n' = #maxName st

                                    val st = {cpsed = IM.insert (#cpsed st, n, n'),
                                              cpsed_range = IM.insert (#cpsed_range st, n', ran),
                                              cps_decls = #cps_decls st,
                                              exported = #exported st,
                                              export_decls = #export_decls st,
                                              maxName = n' + 1}

                                    val unit = (TRecord (CRecord ((KType, loc), []), loc), loc)
                                    val body = (EFfi ("Basis", "bind"), loc)
                                    val body = (ECApp (body, (CFfi ("Basis", "transaction"), loc)), loc)
                                    val body = (ECApp (body, t1), loc)
                                    val body = (ECApp (body, unit), loc)
                                    val body = (EApp (body, (EFfi ("Basis", "transaction_monad"), loc)), loc)
                                    val body = (EApp (body, e), loc)
                                    val body = (EApp (body, (ERel (length args), loc)), loc)
                                    val bt = (CApp ((CFfi ("Basis", "transaction"), loc), unit), loc)
                                    val (body, bt) = foldr (fn ((x, t), (body, bt)) =>
                                                               ((EAbs (x, t, bt, body), loc),
                                                                (TFun (t, bt), loc)))
                                                           (body, bt) fargs
                                    val kt = (TFun (ran, (CApp ((CFfi ("Basis", "transaction"), loc),
                                                                unit),
                                                          loc)), loc)
                                    val body = (EAbs ("k", kt, bt, body), loc)
                                    val bt = (TFun (kt, bt), loc)

                                    val (body, st) = doExp (body, st)

                                    val vi = (name ^ "_cps",
                                              n',
                                              bt,
                                              body,
                                              "")

                                    val st = {cpsed = #cpsed st,
                                              cpsed_range = #cpsed_range st,
                                              cps_decls = vi :: #cps_decls st,
                                              exported = #exported st,
                                              export_decls = #export_decls st,
                                              maxName = #maxName st}
                                in
                                    (makeCall n', st)
                                end
                    end

                fun dummyK loc =
                    let
                        val unit = (TRecord (CRecord ((KType, loc), []), loc), loc)
                                   
                        val k = (EFfi ("Basis", "return"), loc)
                        val k = (ECApp (k, (CFfi ("Basis", "transaction"), loc)), loc)
                        val k = (ECApp (k, unit), loc)
                        val k = (EApp (k, (EFfi ("Basis", "transaction_monad"), loc)), loc)
                        val k = (EApp (k, (ERecord [], loc)), loc)
                    in
                        (EAbs ("_", unit, unit, k), loc)
                    end
            in
                case e of
                    EApp (
                    (EApp
                         ((EApp ((ECApp ((ECApp ((ECApp ((EFfi ("Basis", "bind"), loc), _), _), t1), _), t2), _),
                                 (EFfi ("Basis", "transaction_monad"), _)), _),
                          (ECase (ed, pes, {disc, ...}), _)), _),
                    trans2) =>
                    let
                        val e' = (EFfi ("Basis", "bind"), loc)
                        val e' = (ECApp (e', (CFfi ("Basis", "transaction"), loc)), loc)
                        val e' = (ECApp (e', t1), loc)
                        val e' = (ECApp (e', t2), loc)
                        val e' = (EApp (e', (EFfi ("Basis", "transaction_monad"), loc)), loc)

                        val (pes, st) = ListUtil.foldlMap (fn ((p, e), st) =>
                                                              let
                                                                  val e' = (EApp (e', e), loc)
                                                                  val e' = (EApp (e',
                                                                                  multiLiftExpInExp (E.patBindsN p)
                                                                                                    trans2), loc)
                                                                  val (e', st) = doExp (e', st)
                                                              in
                                                                  ((p, e'), st)
                                                              end) st pes
                    in
                        (ECase (ed, pes, {disc = disc,
                                          result = (CApp ((CFfi ("Basis", "transaction"), loc), t2), loc)}),
                         st)
                    end

                  | EApp (
                    (EApp
                         ((EApp ((ECApp ((ECApp ((ECApp ((EFfi ("Basis", "bind"), loc), _), _), t1), _), t2), _),
                                 (EFfi ("Basis", "transaction_monad"), _)), _),
                          (EServerCall (n, es, ke, t), _)), _),
                    trans2) =>
                    let
                        val e' = (EFfi ("Basis", "bind"), loc)
                        val e' = (ECApp (e', (CFfi ("Basis", "transaction"), loc)), loc)
                        val e' = (ECApp (e', t), loc)
                        val e' = (ECApp (e', t2), loc)
                        val e' = (EApp (e', (EFfi ("Basis", "transaction_monad"), loc)), loc)
                        val e' = (EApp (e', (EApp (E.liftExpInExp 0 ke, (ERel 0, loc)), loc)), loc)
                        val e' = (EApp (e', E.liftExpInExp 0 trans2), loc)
                        val e' = (EAbs ("x", t, t2, e'), loc)
                        val e' = (EServerCall (n, es, e', t), loc)
                        val (e', st) = doExp (e', st)
                    in
                        (#1 e', st)
                    end

                  | EApp (
                    (EApp
                         ((EApp ((ECApp ((ECApp ((ECApp ((EFfi ("Basis", "bind"), loc), _), _), _), _), t3), _),
                                 (EFfi ("Basis", "transaction_monad"), _)), _),
                          (EApp ((EApp
                                      ((EApp ((ECApp ((ECApp ((ECApp ((EFfi ("Basis", "bind"), _), _), _), t1), _), t2), _),
                                              (EFfi ("Basis", "transaction_monad"), _)), _),
                                       trans1), _), trans2), _)), _),
                    trans3) =>
                    let
                        val e'' = (EFfi ("Basis", "bind"), loc)
                        val e'' = (ECApp (e'', (CFfi ("Basis", "transaction"), loc)), loc)
                        val e'' = (ECApp (e'', t2), loc)
                        val e'' = (ECApp (e'', t3), loc)
                        val e'' = (EApp (e'', (EFfi ("Basis", "transaction_monad"), loc)), loc)
                        val e'' = (EApp (e'', (EApp (E.liftExpInExp 0 trans2, (ERel 0, loc)), loc)), loc)
                        val e'' = (EApp (e'', E.liftExpInExp 0 trans3), loc)
                        val e'' = (EAbs ("x", t1, (CApp ((CFfi ("Basis", "transaction"), loc), t3), loc), e''), loc)

                        val e' = (EFfi ("Basis", "bind"), loc)
                        val e' = (ECApp (e', (CFfi ("Basis", "transaction"), loc)), loc)
                        val e' = (ECApp (e', t1), loc)
                        val e' = (ECApp (e', t3), loc)
                        val e' = (EApp (e', (EFfi ("Basis", "transaction_monad"), loc)), loc)
                        val e' = (EApp (e', trans1), loc)
                        val e' = (EApp (e', e''), loc)
                        val (e', st) = doExp (e', st)
                    in
                        (#1 e', st)
                    end

                  | EApp (
                    (EApp
                         ((EApp ((ECApp ((ECApp ((ECApp ((EFfi ("Basis", "bind"), _), _), _), _), _), _), _),
                                 (EFfi ("Basis", "transaction_monad"), _)), _),
                          _), loc),
                    (EAbs (_, _, _, (EWrite _, _)), _)) => (e, st)

                  | EApp (
                    (EApp
                         ((EApp ((ECApp ((ECApp ((ECApp ((EFfi ("Basis", "bind"), _), _), _), t1), _), t2), _),
                                 (EFfi ("Basis", "transaction_monad"), _)), _),
                          trans1), loc),
                    trans2) =>
                    (case (serverSide (#cpsed_range st) trans1, clientSide (#cpsed_range st) trans1,
                           serverSide (#cpsed_range st) trans2, clientSide (#cpsed_range st) trans2) of
                         (true, false, _, true) => newRpc (trans1, trans2, st)
                       | (_, true, true, false) =>
                         (case #1 trans2 of
                              EAbs (x, dom, ran, trans2) =>
                              let
                                  val (trans2, st) = newRpc (trans2, dummyK loc, st)
                                  val trans2 = (EAbs (x, dom, ran, (trans2, loc)), loc)

                                  val e = (EFfi ("Basis", "bind"), loc)
                                  val e = (ECApp (e, (CFfi ("Basis", "transaction"), loc)), loc)
                                  val e = (ECApp (e, t1), loc)
                                  val e = (ECApp (e, t2), loc)
                                  val e = (EApp (e, (EFfi ("Basis", "transaction_monad"), loc)), loc)
                                  val e = (EApp (e, trans1), loc)
                                  val e = EApp (e, trans2)
                              in
                                  (e, st)
                              end
                            | _ => (e, st))
                       | (true, true, _, _) => newCps (t1, t2, trans1, trans2, st)

                       | _ => (e, st))

                  | ERecord xes =>
                    let
                        val loc = case xes of
                                      [] => ErrorMsg.dummySpan
                                    | (_, (_, loc), _) :: _ => loc

                        fun candidate (x, e) =
                            String.isPrefix "On" x
                            andalso serverSide (#cpsed_range st) e
                            andalso not (clientSide (#cpsed_range st) e)
                    in
                        if List.exists (fn ((CName x, _), e, _) => candidate (x, e)
                                         | _ => false) xes then
                            let
                                val (xes, st) = ListUtil.foldlMap
                                                (fn (y as (nm as (CName x, _), e, t), st) =>
                                                    if candidate (x, e) then
                                                        let
                                                            val (e, st) = newRpc (e, dummyK loc, st)
                                                        in
                                                            ((nm, (e, loc), t), st)
                                                        end
                                                    else
                                                        (y, st)
                                                  | y => y)
                                                st xes
                            in
                                (ERecord xes, st)
                            end
                        else
                            (e, st)
                    end

                  | _ => (e, st)
            end

        and doExp (e, st) = U.Exp.foldMap {kind = fn x => x,
                                           con = fn x => x,
                                           exp = exp} st (ReduceLocal.reduceExp e)

        fun decl (d, st : state) =
            let
                val (d, st) = U.Decl.foldMap {kind = fn x => x,
                                              con = fn x => x,
                                              exp = exp,
                                              decl = fn x => x}
                              st d
            in
                (List.revAppend (case #cps_decls st of
                                     [] => [d]
                                   | ds =>
                                     case d of
                                         (DValRec vis, loc) => [(DValRec (ds @ vis), loc)]
                                       | (_, loc) => [d, (DValRec ds, loc)],
                                 #export_decls st),
                 {cpsed = #cpsed st,
                  cpsed_range = #cpsed_range st,
                  cps_decls = [],
                  
                  exported = #exported st,
                  export_decls = [],

                  maxName = #maxName st})
            end

        val (file, _) = ListUtil.foldlMapConcat decl
                        {cpsed = IM.empty,
                         cpsed_range = IM.empty,
                         cps_decls = [],

                         exported = IS.empty,
                         export_decls = [],

                         maxName = U.File.maxName file + 1}
                        file
    in
        file
    end

end
