(* Copyright (c) 2019 Artyom Shalkhakov
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

type endpoint = {Method : method, Url : string, ContentType : string option, LastModified : Time.time option}
type report = {Endpoints : endpoint list}

fun p_endpoint {Method = m, Url = u, ContentType = oct, LastModified = olm} =
    let
        val rfcFmt = "%a, %d %b %Y %H:%M:%S GMT"
    in
        box [string "{",
             string "\"method\": \"", string (methodToString m), string "\", ",
             string "\"url\": \"", string u, string "\", ",
             string "\"content-type\": ", (case oct of SOME ct => box [string "\"", string ct, string"\""]
                                                     | NONE => string "null"),
             string "}"]
    end

fun p_report {Endpoints = el} =
    box [string "{\"endpoints\":",
         space,
         string "[",
         p_list_sep (box [string ",", newline]) p_endpoint el,
         string "]}"]

val endpoints = ref ([] : endpoint list)
val jsFile = ref (NONE : string option)

fun setJavaScript x = jsFile := SOME x

fun reset () = (endpoints := []; jsFile := NONE)

fun collect file =
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
                    {Method = exportKindToMethod ek, Url = id, LastModified = NONE, ContentType = NONE} :: st
                 | _ => st
            end

        val () = reset ()

        val (decls, _) = file
        val ep = foldl decl [] decls

        fun binfile ({Uri = u, ContentType = ct, LastModified = lm, Bytes = _ }, st) =
            {Method = GET, Url = u, LastModified = SOME lm, ContentType = ct} :: st

        val ep = foldl binfile ep (Settings.listFiles ())

        fun jsfile ({Filename = f, Content = _}, st) =
            {Method = GET, Url = f, LastModified = NONE, ContentType = SOME "text/javascript"} :: st

        val ep = foldl jsfile ep (Settings.listJsFiles ())
    in
        endpoints := ep;
        file
    end

fun summarize () =
    let
        val ep = !endpoints
        val js = !jsFile
        val ep =
            case js of
                NONE => ep
             |  SOME js =>
                {Method = GET, Url = js, LastModified = NONE, ContentType = SOME "text/javascript"} :: ep
    in
        {Endpoints = ep}
    end

end
