(* Copyright (c) 2011, Adam Chlipala
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

structure SideCheck :> SIDE_CHECK = struct

open Mono

structure E = ErrorMsg

structure SK = struct
type ord_key = string
val compare = String.compare
end

structure SS = BinarySetFn(SK)

val envVars = ref SS.empty

fun check ds =
    let
        val alreadyWarned = ref false
    in
        envVars := SS.empty;
        MonoUtil.File.appLoc (fn (e, loc) =>
                                 let
                                     fun error (k as (k1, k2)) =
                                         if Settings.isClientOnly k then
                                             let
                                                 val k2 = case k1 of
                                                              "Basis" =>
                                                              (case k2 of
                                                                   "get_client_source" => "get"
                                                                 | _ => k2)
                                                            | _ => k2
                                             in
                                                 E.errorAt loc ("Server-side code uses client-side-only identifier \"" ^ k1 ^ "." ^ k2 ^ "\"")
                                             end
                                         else
                                             ()
                                 in
                                     case e of
                                         EFfi k => error k
                                       | EFfiApp ("Basis", "getenv", [(e, _)]) =>
                                         (case #1 e of
                                              EPrim (Prim.String (_, s)) =>
                                              envVars := SS.add (!envVars, s)
                                            | _ => if !alreadyWarned then
                                                       ()
                                                   else
                                                       (alreadyWarned := true;
                                                        TextIO.output (TextIO.stdErr, "WARNING: " ^ ErrorMsg.spanToString loc ^ ": reading from an environment variable not determined at compile time, which can confuse CSRF protection")))
                                       | EFfiApp (k1, k2, _) => error (k1, k2)
                                       | _ => ()
                                 end) ds;
        ds
    end

fun readEnvVars () = SS.listItems (!envVars)

end
