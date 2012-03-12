(* Copyright (c) 2009, 2012, Adam Chlipala
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

type state = {
     exported : IS.set,
     export_decls : decl list
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
                                 (*Print.preface ("Expression",
                                                CorePrint.p_exp CoreEnv.empty trans);*)
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
                                         (DExport (Rpc ReadWrite, n, false), loc) :: #export_decls st)

                                val st = {exported = exported,
                                          export_decls = export_decls}

                                val e' = EServerCall (n, args, ran)
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
                (d :: #export_decls st,
                 {exported = #exported st,
                  export_decls = []})
            end

        val (file, _) = ListUtil.foldlMapConcat decl
                        {exported = IS.empty,
                         export_decls = []}
                        file
    in
        file
    end

end
