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
     exps : int IM.map,
     decls : (string * int * con * exp * string) list
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
                     in
                         (EServerCall (n, args, trans2), st)
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
                (case #decls st of
                     [] => [d]
                   | ds =>
                     case d of
                         (DValRec vis, loc) => [(DValRec (ds @ vis), loc)]
                       | (_, loc) => [(DValRec ds, loc), d],
                 {decls = [],
                  exps = #exps st})
            end

        val (file, _) = ListUtil.foldlMapConcat decl
                        {decls = [],
                         exps = IM.empty}
                        file
    in
        file
    end

end
