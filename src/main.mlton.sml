(* Copyright (c) 2008, Adam Chlipala
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

fun doArgs (args, (timing, demo, sources)) =
    case args of
        [] => (timing, demo, rev sources)
      | "-demo" :: prefix :: rest =>
        doArgs (rest, (timing, SOME (prefix, false), sources))
      | "-guided-demo" :: prefix :: rest =>
        doArgs (rest, (timing, SOME (prefix, true), sources))
      | arg :: rest =>
        let
            val acc =
                if size arg > 0 andalso String.sub (arg, 0) = #"-" then
                    case arg of
                        "-timing" => (true, demo, sources)
                      | _ => raise Fail ("Unknown option " ^ arg)
                else
                    (timing, demo, arg :: sources)
        in
            doArgs (rest, acc)
        end

val (timing, demo, sources) = doArgs (CommandLine.arguments (), (false, NONE, []))

val job =
    case sources of
        [file] => file
      | _ => raise Fail "Zero or multiple job files specified"

val () =
    case demo of
        SOME (prefix, guided) =>
        Demo.make {prefix = prefix, dirname = job, guided = guided}
      | NONE =>
        if timing then
            Compiler.time Compiler.toCjrize job
        else
            Compiler.compile job
