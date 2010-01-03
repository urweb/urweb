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

open Cjr

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)
structure IS = IntBinarySet

val pullBasis = SS.addList (SS.empty,
                            ["new_client_source",
                             "get_client_source",
                             "set_client_source"])

val pushBasis = SS.addList (SS.empty,
                            ["new_channel",
                             "self"])

val events = ["abort",
              "blur",
              "change",
              "click",
              "dblclick",
              "error",
              "focus",
              "keydown",
              "keypress",
              "keyup",
              "load",
              "mousedown",
              "mousemove",
              "mouseout",
              "mouseover",
              "mouseup",
              "reset",
              "resize",
              "select",
              "submit",
              "unload"]
                
val scriptWords = "<script"
                   :: map (fn s => " on" ^ s ^ "='") events

val pushWords = ["rv("]

fun classify (ds, ps) =
    let
        val proto = Settings.currentProtocol ()

        fun inString {needle, haystack} = String.isSubstring needle haystack

        fun hasClient {basis, words, onload} csids =
            let
                fun hasClient e =
                    case #1 e of
                        EPrim (Prim.String s) => List.exists (fn n => inString {needle = n, haystack = s}) words
                      | EPrim _ => false
                      | ERel _ => false
                      | ENamed n => IS.member (csids, n)
                      | ECon (_, _, NONE) => false
                      | ECon (_, _, SOME e) => hasClient e
                      | ENone _ => false
                      | ESome (_, e) => hasClient e
                      | EFfi ("Basis", x) => SS.member (basis, x)
                      | EFfi _ => false
                      | EFfiApp ("Basis", "maybe_onload",
                                 [(EFfiApp ("Basis", "strcat", all as [_, (EPrim (Prim.String s), _)]), _)]) =>
                        List.exists hasClient all
                        orelse (onload andalso size s > 0)
                      | EFfiApp ("Basis", x, es) => SS.member (basis, x)
                                                    orelse List.exists hasClient es
                      | EFfiApp (_, _, es) => List.exists hasClient es
                      | EApp (e, es) => hasClient e orelse List.exists hasClient es
                      | EUnop (_, e) => hasClient e
                      | EBinop (_, e1, e2) => hasClient e1 orelse hasClient e2
                      | ERecord (_, xes) => List.exists (hasClient o #2) xes
                      | EField (e, _) => hasClient e
                      | ECase (e, pes, _) => hasClient e orelse List.exists (hasClient o #2) pes
                      | EError (e, _) => hasClient e
                      | EReturnBlob {blob = e1, mimeType = e2, ...} => hasClient e1 orelse hasClient e2
                      | ERedirect (e, _) => hasClient e
                      | EWrite e => hasClient e
                      | ESeq (e1, e2) => hasClient e1 orelse hasClient e2
                      | ELet (_, _, e1, e2) => hasClient e1 orelse hasClient e2
                      | EQuery {query, body, initial, ...} => hasClient query orelse hasClient body
                                                              orelse hasClient initial
                      | EDml {dml, ...} => hasClient dml
                      | ENextval {seq, ...} => hasClient seq
                      | ESetval {seq, count, ...} => hasClient seq orelse hasClient count
                      | EUnurlify (e, _, _) => hasClient e
            in
                hasClient
            end

        fun decl ((d, _), (pull_ids, push_ids)) =
            let
                val hasClientPull = hasClient {basis = pullBasis, words = scriptWords, onload = true} pull_ids
                val hasClientPush = hasClient {basis = pushBasis, words = pushWords, onload = false} push_ids
            in
                case d of
                    DVal (_, n, _, e) => (if hasClientPull e then
                                             IS.add (pull_ids, n)
                                          else
                                              pull_ids,
                                          if hasClientPush e then
                                              IS.add (push_ids, n)
                                          else
                                              push_ids)
                  | DFun (_, n, _, _, e) => (if hasClientPull e then
                                                 IS.add (pull_ids, n)
                                             else
                                                 pull_ids,
                                             if hasClientPush e then
                                                 IS.add (push_ids, n)
                                             else
                                                 push_ids)
                  | DFunRec xes => (if List.exists (fn (_, _, _, _, e) => hasClientPull e) xes then
                                       foldl (fn ((_, n, _, _, _), pull_ids) => IS.add (pull_ids, n))
                                             pull_ids xes
                                    else
                                        pull_ids,
                                    if List.exists (fn (_, _, _, _, e) => hasClientPush e) xes then
                                        foldl (fn ((_, n, _, _, _), push_ids) => IS.add (push_ids, n))
                                              push_ids xes
                                    else
                                        push_ids)
                  | _ => (pull_ids, push_ids)
            end

        val (pull_ids, push_ids) = foldl decl (IS.empty, IS.empty) ds

        val foundBad = ref false

        val ps = map (fn (ek, x, n, ts, t, _, b) =>
                         (ek, x, n, ts, t,
                          if IS.member (push_ids, n) then
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
                              ServerOnly,
                          b)) ps
    in
        (ds, ps)
    end

end

