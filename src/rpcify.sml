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

val ssBasis = SS.addList (SS.empty,
                          ["requestHeader",
                           "query",
                           "dml",
                           "nextval"])

val csBasis = SS.addList (SS.empty,
                          ["source",
                           "get",
                           "set",
                           "alert"])

type state = {
     cpsed : int IM.map,
     cps_decls : (string * int * con * exp * string) list,

     exported : IS.set,
     export_decls : decl list
}

fun frob file =
    let
        fun sideish (basis, ssids) =
            U.Exp.exists {kind = fn _ => false,
                          con = fn _ => false,
                          exp = fn ENamed n => IS.member (ssids, n)
                                 | EFfi ("Basis", x) => SS.member (basis, x)
                                 | EFfiApp ("Basis", x, _) => SS.member (basis, x)
                                 | _ => false}

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

        val serverSide = sideish (ssBasis, ssids)
        val clientSide = sideish (csBasis, csids)

        val tfuncs = foldl
                     (fn ((d, _), tfuncs) =>
                         let
                             fun doOne ((_, n, t, _, _), tfuncs) =
                                 let
                                     fun crawl ((t, _), args) =
                                         case t of
                                             CApp ((CFfi ("Basis", "transaction"), _), ran) => SOME (rev args, ran)
                                           | TFun (arg, rest) => crawl (rest, arg :: args)
                                           | _ => NONE
                                 in
                                     case crawl (t, []) of
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
            case e of
                EApp (
                (EApp
                     ((EApp ((ECApp ((ECApp ((ECApp ((EFfi ("Basis", "bind"), loc), _), _), t1), _), t2), _),
                             (EFfi ("Basis", "transaction_monad"), _)), _),
                      trans1), _),
                trans2) =>
                (case (serverSide trans1, clientSide trans1, serverSide trans2, clientSide trans2) of
                     (true, false, false, _) =>
                     let
                         fun getApp (e, args) =
                             case #1 e of
                                 ENamed n => (n, args)
                               | EApp (e1, e2) => getApp (e1, e2 :: args)
                               | _ => (ErrorMsg.errorAt loc "Mixed client/server code doesn't use a named function for server part";
                                       (0, []))

                         val (n, args) = getApp (trans1, [])

                         val (exported, export_decls) =
                             if IS.member (#exported st, n) then
                                 (#exported st, #export_decls st)
                             else
                                 (IS.add (#exported st, n),
                                  (DExport (Rpc, n), loc) :: #export_decls st)

                         val st = {cpsed = #cpsed st,
                                   cps_decls = #cps_decls st,

                                   exported = exported,
                                   export_decls = export_decls}

                         val ran =
                             case IM.find (tfuncs, n) of
                                 NONE => raise Fail "Rpcify: Undetected transaction function"
                               | SOME (_, ran) => ran
                     in
                         (EServerCall (n, args, trans2, ran), st)
                     end
                   | _ => (e, st))
              | _ => (e, st)

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
                  cps_decls = [],
                  
                  exported = #exported st,
                  export_decls = []})
            end

        val (file, _) = ListUtil.foldlMapConcat decl
                        {cpsed = IM.empty,
                         cps_decls = [],

                         exported = IS.empty,
                         export_decls = []}
                        file
    in
        file
    end

end
