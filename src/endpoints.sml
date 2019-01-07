(* Copyright (c) 2010, 2013, Adam Chlipala
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

structure Endpoints :> ENDPOINTS = struct

open Print.PD
open Print

open Mono

datatype method = GET | POST

fun methodToString GET = "GET"
  | methodToString POST = "POST"

type endpoint = {Method : method, Url : string}
type report = {Endpoints : endpoint list}

fun p_endpoint {Method = m, Url = u} =
    box [string "{",
         string "\"method\": \"", string (methodToString m), string "\",",
         string "\"url\": \"", string u, string "\"",
         string "}"]

fun p_report {Endpoints = el} =
    box [string "{\"endpoints\":",
         space,
         string "[",
         p_list_sep (box [string ",", newline]) p_endpoint el,
         string "]}"]

fun summarize file =
    let
        fun exportKindToMethod (Link _) = GET
          | exportKindToMethod (Action _) = POST
          | exportKindToMethod (Rpc _) = POST
          | exportKindToMethod (Extern _) = POST

        fun decl ((d, _), st as endpoints) =
            let
            in
                case d of
                    DExport (ek, id, i, tl, rt, f) =>
                    {Method = exportKindToMethod ek, Url = id} :: st
                 | _ => st
            end

        val (decls, _) = file
        val ep = foldl decl [] decls
    in
        {Endpoints = ep}
    end

end
