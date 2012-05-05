(* Copyright (c) 2008-2012, Adam Chlipala
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

val socket = ".urweb_daemon"

(* Encapsulate main invocation handler in a function, possibly to be called multiple times within a daemon. *)

exception Code of OS.Process.status

fun oneRun args =
    let
        val timing = ref false
        val tc = ref false
        val sources = ref ([] : string list)
        val demo = ref (NONE : (string * bool) option)
        val tutorial = ref false
        val css = ref false

        val () = (Compiler.debug := false;
                  Elaborate.verbose := false;
                  Elaborate.dumpTypes := false;
                  Elaborate.dumpTypesOnError := false;
                  Elaborate.unifyMore := false;
                  Compiler.dumpSource := false;
                  Compiler.doIflow := false;
                  Demo.noEmacs := false;
                  Settings.setDebug false)

        val () = Compiler.beforeC := MLton.GC.pack

        fun printVersion () = (print (Config.versionString ^ "\n");
		               raise Code OS.Process.success)
        fun printNumericVersion () = (print (Config.versionNumber ^ "\n");
			              raise Code OS.Process.success)

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
                 Elaborate.verbose := true;
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
              | "-dumpTypesOnError" :: rest =>
                (Elaborate.dumpTypesOnError := true;
                 doArgs rest)
              | "-unifyMore" :: rest =>
                (Elaborate.unifyMore := true;
                 doArgs rest)
              | "-dumpSource" :: rest =>
                (Compiler.dumpSource := true;
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
              | "-boot" :: rest =>
                (Compiler.enableBoot ();
                 Settings.setStaticLinking true;
                 doArgs rest)
              | "-sigfile" :: name :: rest =>
                (Settings.setSigFile (SOME name);
                 doArgs rest)
              | "-iflow" :: rest =>
                (Compiler.doIflow := true;
                 doArgs rest)
              | "-moduleOf" :: fname :: _ =>
                (print (Compiler.moduleOf fname ^ "\n");
                 raise Code OS.Process.success)
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

        val () = case args of
                     ["daemon", "stop"] => OS.Process.exit OS.Process.success
                   | _ => ()

        val () = doArgs args

        val job =
            case !sources of
                [file] => file
              | _ => printVersion ()
    in
        case (!css, !demo, !tutorial) of
            (true, _, _) =>
            (case Compiler.run Compiler.toCss job of
                 NONE => OS.Process.failure
               | SOME {Overall = ov, Classes = cl} =>
                 (app (print o Css.inheritableToString) ov;
                  print "\n";
                  app (fn (x, (ins, ots)) =>
                          (print x;
                           print " ";
                           app (print o Css.inheritableToString) ins;
                           app (print o Css.othersToString) ots;
                           print "\n")) cl;
                  OS.Process.success))
          | (_, SOME (prefix, guided), _) =>
            if Demo.make' {prefix = prefix, dirname = job, guided = guided} then
                OS.Process.success
            else
                OS.Process.failure
          | (_, _, true) => (Tutorial.make job;
                             OS.Process.success)
          | _ =>
            if !tc then
                (Compiler.check Compiler.toElaborate job;
                 if ErrorMsg.anyErrors () then
                     OS.Process.failure
                 else
                     OS.Process.success)
            else if !timing then
                (Compiler.time Compiler.toCjrize job;
                 OS.Process.success)
            else
                (if Compiler.compile job then
                     OS.Process.success
                 else
                     OS.Process.failure)
    end handle Code n => n

fun send (sock, s) =
    let
        val n = Socket.sendVec (sock, Word8VectorSlice.full (Vector.map (Word8.fromInt o ord) s))
    in
        if n >= size s then
            ()
        else
            send (sock, String.extract (s, n, NONE))
    end

val () = case CommandLine.arguments () of
             ["daemon", "start"] =>
             (case Posix.Process.fork () of
                  SOME _ => ()
                | NONE =>
                  let
                      val () = Elaborate.incremental := true
                      val listen = UnixSock.Strm.socket ()

                      fun loop () =
                          let
                              val (sock, _) = Socket.accept listen

                              fun loop' (buf, args) =
                                  let
                                      val s = if CharVector.exists (fn ch => ch = #"\n") buf then
                                                  ""
                                              else
                                                  Vector.map (chr o Word8.toInt) (Socket.recvVec (sock, 1024))
                                      val s = buf ^ s
                                      val (befor, after) = Substring.splitl (fn ch => ch <> #"\n") (Substring.full s)
                                  in
                                      if Substring.isEmpty after then
                                          loop' (s, args)
                                      else
                                          let
                                              val cmd = Substring.string befor
                                              val rest = Substring.string (Substring.slice (after, 1, NONE))
                                          in
                                              case cmd of
                                                  "" =>
                                                  let
                                                      val success = (oneRun (rev args))
                                                          handle ex => (print "unhandled exception:\n";
                                                                        print (General.exnMessage ex ^ "\n");
                                                                        OS.Process.failure)
                                                  in
                                                      TextIO.flushOut TextIO.stdOut;
                                                      TextIO.flushOut TextIO.stdErr;
                                                      send (sock, if OS.Process.isSuccess success then
                                                                      "\001"
                                                                  else
                                                                      "\002")
                                                  end
                                                | _ => loop' (rest, cmd :: args)
                                          end
                                  end handle OS.SysErr _ => ()

                              fun redirect old =
                                  Posix.IO.dup2 {old = valOf (Posix.FileSys.iodToFD (Socket.ioDesc sock)),
                                                 new = old}

                              val oldStdout = Posix.IO.dup Posix.FileSys.stdout
                              val oldStderr = Posix.IO.dup Posix.FileSys.stderr
                          in
                              (* Redirect the daemon's output to the socket. *)
                              redirect Posix.FileSys.stdout;
                              redirect Posix.FileSys.stderr;
                              
                              loop' ("", []);
                              Socket.close sock;

                              Posix.IO.dup2 {old = oldStdout, new = Posix.FileSys.stdout};
                              Posix.IO.dup2 {old = oldStderr, new = Posix.FileSys.stderr};
                              Posix.IO.close oldStdout;
                              Posix.IO.close oldStderr;

                              MLton.GC.pack ();
                              loop ()
                          end
                  in
                      OS.Process.atExit (fn () => OS.FileSys.remove socket);
                      Socket.bind (listen, UnixSock.toAddr socket);
                      Socket.listen (listen, 1);
                      loop ()
                  end)

           | args =>
             let
                 val sock = UnixSock.Strm.socket ()

                 fun wait () =
                     let
                         val v = Socket.recvVec (sock, 1024)
                     in
                         if Vector.length v = 0 then
                             OS.Process.failure
                         else
                             let
                                 val s = Vector.map (chr o Word8.toInt) v
                                 val last = Vector.sub (v, Vector.length v - 1)
                                 val (rc, s) = if last = Word8.fromInt 1 then
                                                   (SOME OS.Process.success, String.substring (s, 0, size s - 1))
                                               else if last = Word8.fromInt 2 then
                                                   (SOME OS.Process.failure, String.substring (s, 0, size s - 1))
                                               else
                                                   (NONE, s)
                             in
                                 print s;
                                 case rc of
                                     NONE => wait ()
                                   | SOME rc => rc
                             end
                     end handle OS.SysErr _ => OS.Process.failure
             in
                 if Socket.connectNB (sock, UnixSock.toAddr socket)
                    orelse not (List.null (#wrs (Socket.select {rds = [],
                                                                wrs = [Socket.sockDesc sock],
                                                                exs = [],
                                                                timeout = SOME (Time.fromSeconds 1)}))) then
                     (app (fn arg => send (sock, arg ^ "\n")) args;
                      send (sock, "\n");
                      OS.Process.exit (wait ()))
                 else
                     (OS.FileSys.remove socket;
                      raise OS.SysErr ("", NONE))
             end handle OS.SysErr _ => case args of
                                           ["daemon", "stop"] => (OS.FileSys.remove socket handle OS.SysErr _ => ())
                                         | _ => OS.Process.exit (oneRun args)
