(* Copyright (c) 2008-2011, Adam Chlipala
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

val timing = ref false
val tc = ref false
val sources = ref ([] : string list)
val demo = ref (NONE : (string * bool) option)
val tutorial = ref false
val css = ref false

val () = Compiler.beforeC := MLton.GC.pack

fun printVersion () = (print (Config.versionString ^ "\n");
		       OS.Process.exit OS.Process.success)
fun printNumericVersion () = (print (Config.versionNumber ^ "\n");
			      OS.Process.exit OS.Process.success)

fun doArgs args =
    case args of
        [] => ()
      | "-version" :: rest => 
	printVersion ()
      | "-numeric-version" :: rest =>
	printNumericVersion ()
      | "-css" :: rest =>
        (css := true;
         doArgs rest)
      | "-demo" :: prefix :: rest =>
        (demo := SOME (prefix, false);
         doArgs rest)
      | "-guided-demo" :: prefix :: rest =>
        (demo := SOME (prefix, true);
         doArgs rest)
      | "-tutorial" :: rest =>
        (tutorial := true;
         doArgs rest)
      | "-protocol" :: name :: rest =>
        (Settings.setProtocol name;
         doArgs rest)
      | "-prefix" :: prefix :: rest =>
        (Settings.setUrlPrefix prefix;
         doArgs rest)
      | "-db" :: s :: rest =>
        (Settings.setDbstring (SOME s);
         doArgs rest)
      | "-dbms" :: name :: rest =>
        (Settings.setDbms name;
         doArgs rest)
      | "-debug" :: rest =>
        (Settings.setDebug true;
         doArgs rest)
      | "-verbose" :: rest =>
        (Compiler.debug := true;
         doArgs rest)
      | "-timing" :: rest =>
        (timing := true;
         doArgs rest)
      | "-tc" :: rest =>
        (tc := true;
         doArgs rest)
      | "-dumpTypes" :: rest =>
        (Elaborate.dumpTypes := true;
         doArgs rest)
      | "-output" :: s :: rest =>
        (Settings.setExe (SOME s);
         doArgs rest)
      | "-sql" :: s :: rest =>
        (Settings.setSql (SOME s);
         doArgs rest)
      | "-static" :: rest =>
        (Settings.setStaticLinking true;
         doArgs rest)
      | "-path" :: name :: path :: rest =>
        (Compiler.addPath (name, path);
         doArgs rest)
      | "-root" :: name :: root :: rest =>
        (Compiler.addModuleRoot (root, name);
         doArgs rest)
      | "-sigfile" :: name :: rest =>
        (Settings.setSigFile (SOME name);
         doArgs rest)
      | "-iflow" :: rest =>
        (Compiler.doIflow := true;
         doArgs rest)
      | "-moduleOf" :: fname :: _ =>
        (print (Compiler.moduleOf fname ^ "\n");
         OS.Process.exit OS.Process.success)
      | "-noEmacs" :: rest =>
        (Demo.noEmacs := true;
         doArgs rest)
      | "-limit" :: class :: num :: rest =>
        (case Int.fromString num of
             NONE => raise Fail ("Invalid limit number '" ^ num ^ "'")
           | SOME n =>
             if n < 0 then
                 raise Fail ("Invalid limit number '" ^ num ^ "'")
             else
                 Settings.addLimit (class, n);
         doArgs rest)
      | arg :: rest =>
        (if size arg > 0 andalso String.sub (arg, 0) = #"-" then
             raise Fail ("Unknown flag " ^ arg)
         else
             sources := arg :: !sources;
         doArgs rest)

val () = doArgs (CommandLine.arguments ())

val job =
    case !sources of
        [file] => file
      | _ => printVersion ()

val () =
    case (!css, !demo, !tutorial) of
        (true, _, _) =>
        (case Compiler.run Compiler.toCss job of
             NONE => OS.Process.exit OS.Process.failure
           | SOME {Overall = ov, Classes = cl} =>
             (app (print o Css.inheritableToString) ov;
              print "\n";
              app (fn (x, (ins, ots)) =>
                      (print x;
                       print " ";
                       app (print o Css.inheritableToString) ins;
                       app (print o Css.othersToString) ots;
                       print "\n")) cl))
      | (_, SOME (prefix, guided), _) =>
        Demo.make {prefix = prefix, dirname = job, guided = guided}
      | (_, _, true) => Tutorial.make job
      | _ =>
        if !tc then
            (Compiler.check Compiler.toElaborate job;
             if ErrorMsg.anyErrors () then
                 OS.Process.exit OS.Process.failure
             else
                 ())
        else if !timing then
            Compiler.time Compiler.toCjrize job
        else
            Compiler.compiler job
