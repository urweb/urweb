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

val csBasis = SS.addList (SS.empty,
                          ["new_client_source",
                           "get_client_source",
                           "set_client_source"])

val scriptWords = ["<script",
                   " onclick="]

fun classify (ds, ps) =
    let
        fun inString {needle, haystack} =
            let
                val (_, suffix) = Substring.position needle (Substring.full haystack)
            in
                not (Substring.isEmpty suffix)
            end

        fun hasClient csids =
            let
                fun hasClient e =
                    case #1 e of
                        EPrim (Prim.String s) => List.exists (fn n => inString {needle = n, haystack = s}) scriptWords
                      | EPrim _ => false
                      | ERel _ => false
                      | ENamed n => IS.member (csids, n)
                      | ECon (_, _, NONE) => false
                      | ECon (_, _, SOME e) => hasClient e
                      | ENone _ => false
                      | ESome (_, e) => hasClient e
                      | EFfi ("Basis", x) => SS.member (csBasis, x)
                      | EFfi _ => false
                      | EFfiApp ("Basis", x, es) => SS.member (csBasis, x)
                                                    orelse List.exists hasClient es
                      | EFfiApp (_, _, es) => List.exists hasClient es
                      | EApp (e, es) => hasClient e orelse List.exists hasClient es
                      | EUnop (_, e) => hasClient e
                      | EBinop (_, e1, e2) => hasClient e1 orelse hasClient e2
                      | ERecord (_, xes) => List.exists (hasClient o #2) xes
                      | EField (e, _) => hasClient e
                      | ECase (e, pes, _) => hasClient e orelse List.exists (hasClient o #2) pes
                      | EError (e, _) => hasClient e
                      | EWrite e => hasClient e
                      | ESeq (e1, e2) => hasClient e1 orelse hasClient e2
                      | ELet (_, _, e1, e2) => hasClient e1 orelse hasClient e2
                      | EQuery {query, body, initial, ...} => hasClient query orelse hasClient body
                                                              orelse hasClient initial
                      | EDml {dml, ...} => hasClient dml
                      | ENextval {seq, ...} => hasClient seq
                      | EUnurlify (e, _) => hasClient e
            in
                hasClient
            end

        fun decl ((d, _), csids) =
            let
                val hasClient = hasClient csids
            in
                case d of
                    DVal (_, n, _, e) => if hasClient e then
                                             IS.add (csids, n)
                                         else
                                             csids
                  | DFun (_, n, _, _, e) => if hasClient e then
                                                IS.add (csids, n)
                                            else
                                                csids
                  | DFunRec xes => if List.exists (fn (_, _, _, _, e) => hasClient e) xes then
                                       foldl (fn ((_, n, _, _, _), csids) => IS.add (csids, n))
                                             csids xes
                                   else
                                       csids
                  | _ => csids
            end

        val csids = foldl decl IS.empty ds

        val ps = map (fn (ek, x, n, ts, t, _) =>
                         (ek, x, n, ts, t,
                          if IS.member (csids, n) then
                              ServerAndClient
                          else
                              ServerOnly)) ps
    in
        (ds, ps)
    end

end

