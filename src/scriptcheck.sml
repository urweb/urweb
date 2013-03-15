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

structure ScriptCheck :> SCRIPT_CHECK = struct

open Mono

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)
structure IS = IntBinarySet

val pushBasis = SS.addList (SS.empty,
                            ["new_channel",
                             "self"])

fun classify (ds, ps) =
    let
        val proto = Settings.currentProtocol ()

        fun inString {needle, haystack} = String.isSubstring needle haystack

        fun hasClient {basis, funcs, push} =
            MonoUtil.Exp.exists {typ = fn _ => false,
                                 exp = fn ERecv _ => push
                                        | EFfiApp ("Basis", x, _) => SS.member (basis, x) 
                                        | EJavaScript _ => not push
                                        | ENamed n => IS.member (funcs, n)
                                        | _ => false}

        fun decl ((d, _), (pull_ids, push_ids)) =
            let
                val hasClientPull = hasClient {basis = SS.empty, funcs = pull_ids, push = false}
                val hasClientPush = hasClient {basis = pushBasis, funcs = push_ids, push = true}
            in
                case d of
                    DVal (_, n, _, e, _) => (if hasClientPull e then
                                             IS.add (pull_ids, n)
                                          else
                                              pull_ids,
                                          if hasClientPush e then
                                              IS.add (push_ids, n)
                                          else
                                              push_ids)
                  | DValRec xes => (if List.exists (fn (_, _, _, e, _) => hasClientPull e) xes then
                                       foldl (fn ((_, n, _, _, _), pull_ids) => IS.add (pull_ids, n))
                                             pull_ids xes
                                    else
                                        pull_ids,
                                    if List.exists (fn (_, _, _, e, _) => hasClientPush e) xes then
                                        foldl (fn ((_, n, _, _, _), push_ids) => IS.add (push_ids, n))
                                              push_ids xes
                                    else
                                        push_ids)
                  | _ => (pull_ids, push_ids)
            end

        val (pull_ids, push_ids) = foldl decl (IS.empty, IS.empty) ds

        val foundBad = ref false

        val all_ids = IS.union (pull_ids, push_ids)

        val ps = map (fn n =>
                         (n, if IS.member (push_ids, n) then
                                 (if not (#persistent proto) andalso not (!foundBad) then
                                      (foundBad := true;
                                       ErrorMsg.error ("This program needs server push, but the current protocol ("
                                                       ^ #name proto ^ ") doesn't support that."))
                                  else
                                      ();
                                  ServerAndPullAndPush)
                             else if IS.member (pull_ids, n) then
                                 ServerAndPull
                             else
                                 ServerOnly)) (IS.listItems all_ids)
    in
        (ds, ps)
    end

end

