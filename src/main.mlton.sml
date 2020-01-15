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

exception Code of OS.Process.status

datatype flag_arity =
      ZERO of (unit -> unit)
    | ONE  of string * (string -> unit)
    | TWO  of string * string * (string * string -> unit)

fun parse_flags flag_info args =
    let
        fun search_pred flag0 =
            (* Remove preceding "-". *)
            let val flag0 = String.extract (flag0, 1, NONE)
            in
                fn (flag1, _, _) => flag0 = flag1
            end

        fun normalizeArg arg =
          case arg of
              "-h" => "-help"
            | "--h" => "-help"
            | "--help" => "-help"
            | _ => arg

        fun loop [] : string list = []
          | loop (arg :: args) =
            let
                val arg = normalizeArg arg
            in
                if String.isPrefix "-" arg then
                    case List.find (search_pred arg) flag_info of
                        NONE => raise Fail ("Unknown flag "^arg^", see -help")
                      | SOME x => exec x args
                else
                    arg :: loop args
            end

        and exec (_, ZERO f, _) args =
                (f (); loop args)
          | exec (_, ONE (_, f), _) (x :: args) =
                (f x; loop args)
          | exec (_, TWO (_, _, f), _) (x :: y :: args) =
                (f (x, y); loop args)
          | exec (flag, ONE _, _) [] =
                raise Fail ("Flag "^flag^" is missing an argument, see -help")
          | exec (flag, TWO _, _) [] =
                raise Fail ("Flag "^flag^" is missing two arguments, see -help")
          | exec (flag, TWO _, _) [_] =
                raise Fail ("Flag "^flag^" is missing an argument, see -help")
    in
        loop args
    end

fun usage flag_info =
    let
        val name = CommandLine.name ()

        fun print_desc NONE = print "\n"
          | print_desc (SOME s) = (print " : "; print s; print "\n")

        fun print_args (ZERO _) = ()
          | print_args (ONE (x, _)) = print (" " ^ x)
          | print_args (TWO (x, y, _)) = print (" " ^ x ^ " " ^ y)

        fun print_flag (flag, args, desc) =
            (print ("  -" ^ flag);
             print_args args;
             print_desc desc)
    in
        print "usage: \n";
        print ("  " ^ name ^ " daemon [stop|start]\n");
        print ("  " ^ name ^ " [flag ...] project-name\n");
        print "Supported flags are:\n";
        app print_flag flag_info;
        raise Code OS.Process.success
    end



(* Encapsulate main invocation handler in a function, possibly to be called multiple times within a daemon. *)

exception DaemonExit

fun oneRun args =
    let
        val timing = ref false
        val tc = ref false
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

        fun print_and_exit msg () =
            (print msg; print "\n";
             raise Code OS.Process.success)

        val printVersion = print_and_exit Config.versionString
        val printNumericVersion = print_and_exit Config.versionNumber
        fun printCCompiler () = print_and_exit (Settings.getCCompiler ()) ()
        val printCInclude = print_and_exit Config.includ

        fun printModuleOf fname =
            print_and_exit (Compiler.moduleOf fname) ()

        fun add_class (class, num) =
            case Int.fromString num of
                 NONE => raise Fail ("Invalid limit number '" ^ num ^ "'")
               | SOME n =>
                 if n < 0 then
                     raise Fail ("Invalid limit number '" ^ num ^ "'")
                 else
                     Settings.addLimit (class, n)

        fun set_true flag = ZERO (fn () => flag := true)
        fun call_true f = ZERO (fn () => f true)

        (* This is a function, and not simply a value, because it
         * is recursive in the help-flag. *)
        fun flag_info () = [
              ("help", ZERO (fn () => usage (flag_info ())),
                    SOME "print this overview"),
              ("version", ZERO printVersion,
                    SOME "print version number and exit"),
              ("numeric-version", ZERO printNumericVersion,
                    SOME "print numeric version number and exit"),
              ("css", set_true css,
                    SOME "print categories of CSS properties"),
              ("print-ccompiler", ZERO printCCompiler,
                    SOME "print C compiler and exit"),
              ("print-cinclude", ZERO printCInclude,
                    SOME "print directory of C headers and exit"),
              ("ccompiler", ONE ("<program>", Settings.setCCompiler),
                    SOME "set the C compiler to <program>"),
              ("demo", ONE ("<prefix>", fn prefix =>
                                demo := SOME (prefix, false)),
                    NONE),
              ("guided-demo", ONE ("<prefix>", fn prefix =>
                                demo := SOME (prefix, true)),
                    NONE),
              ("tutorial", set_true tutorial,
                    SOME "render HTML tutorials from .ur source files"),
              ("protocol", ONE ("[http|cgi|fastcgi|static]",
                                Settings.setProtocol),
                    SOME "set server protocol"),
              ("prefix", ONE ("<prefix>", Settings.setUrlPrefix),
                    SOME "set prefix used before all URI's"),
              ("db", ONE ("<string>", Settings.setDbstring o SOME),
                    SOME "database connection information"),
              ("dbms", ONE ("[sqlite|mysql|postgres]", Settings.setDbms),
                    SOME "select database engine"),
              ("debug", call_true Settings.setDebug,
                    SOME "save some intermediate C files"),
              ("verbose", ZERO (fn () =>
                                (Compiler.debug := true;
                                 Elaborate.verbose := true)),
                    NONE),
              ("timing", set_true timing,
                    SOME "time compilation phases"),
              ("tc", set_true tc,
                    SOME "stop after type checking"),
              ("dumpTypes", set_true Elaborate.dumpTypes,
                    SOME "print kinds and types"),
              ("dumpTypesOnError", set_true Elaborate.dumpTypesOnError,
                    SOME "print kinds and types if there is an error"),
              ("unifyMore", set_true Elaborate.unifyMore,
                    SOME "continue unification before reporting type error"),
              ("dumpSource", set_true Compiler.dumpSource,
                    SOME ("print source code of last intermediate program "^
                          "if there is an error")),
              ("dumpVerboseSource", ZERO (fn () =>
                                (Compiler.dumpSource := true;
                                 ElabPrint.debug := true;
                                 ExplPrint.debug := true;
                                 CorePrint.debug := true;
                                 MonoPrint.debug := true)),
                    NONE),
              ("output", ONE ("<file>", Settings.setExe o SOME),
                    SOME "output executable as <file>"),
              ("js", ONE ("<file>", Settings.setOutputJsFile o SOME),
                    SOME "serve JavaScript as <file>"),
              ("sql", ONE ("<file>", Settings.setSql o SOME),
                    SOME "output sql script as <file>"),
              ("endpoints", ONE ("<file>", Settings.setEndpoints o SOME),
                    SOME "output exposed URL endpoints in JSON as <file>"),
              ("static", call_true Settings.setStaticLinking,
                    SOME "enable static linking"),
              ("stop", ONE ("<phase>", Compiler.setStop),
                    SOME "stop compilation after <phase>"),
              ("path", TWO ("<name>", "<path>", Compiler.addPath),
                    SOME ("set path variable <name> to <path> for use in "^
                          ".urp files")),
              ("root", TWO ("<name>", "<path>",
                            (fn (name, path) =>
                                Compiler.addModuleRoot (path, name))),
                    SOME "prefix names of modules found in <path> with <name>"),
              ("boot", ZERO (fn () =>
                            (Compiler.enableBoot ();
                             Settings.setBootLinking true)),
                    SOME ("run from build tree and generate statically linked "^
                          "executables ")),
              ("sigfile", ONE ("<file>", Settings.setSigFile o SOME),
                    SOME "search for cryptographic signing keys in <file>"),
              ("iflow", set_true Compiler.doIflow,
                    NONE),
              ("sqlcache", call_true Settings.setSqlcache,
                    NONE),
              ("heuristic", ONE ("<h>", Sqlcache.setHeuristic),
                    NONE),
              ("moduleOf", ONE ("<file>", printModuleOf),
                    SOME "print module name of <file> and exit"),
              ("startLspServer", ZERO Lsp.startServer, SOME "Start Language Server Protocol server"),
              ("noEmacs", set_true Demo.noEmacs,
                    NONE),
              ("limit", TWO ("<class>", "<num>", add_class),
                    SOME "set resource usage limit for <class> to <num>"),
              ("explainEmbed", set_true JsComp.explainEmbed,
                    SOME ("explain errors about embedding of server-side "^
                          "values in client code"))
        ]

        val () = case args of
                     ["daemon", "stop"] => (OS.FileSys.remove socket handle OS.SysErr _ => ();
                                            raise DaemonExit)
                   | _ => ()

        val sources = parse_flags (flag_info ()) args

        val job =
            case sources of
                [file] => file
              | [] =>
                    raise Fail "No project specified, see -help"
              | files =>
                    raise Fail ("Multiple projects specified;"^
                                " only one is allowed.\nSpecified projects: "^
                                String.concatWith ", " files)
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
        val n = Socket.sendVec (sock, Word8VectorSlice.full (MLton.Word8Vector.fromPoly (Vector.map (Word8.fromInt o ord) (MLton.CharVector.toPoly s))))
    in
        if n >= size s then
            ()
        else
            send (sock, String.extract (s, n, NONE))
    end

fun startDaemon () =
    if OS.FileSys.access (socket, []) then
        (print ("It looks like a daemon is already listening in this directory,\n"
                ^ "though it's possible a daemon died without cleaning up its socket.\n");
         OS.Process.exit OS.Process.failure)
    else case Posix.Process.fork () of
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
                                             MLton.CharVector.fromPoly (Vector.map (chr o Word8.toInt) (MLton.Word8Vector.toPoly (Socket.recvVec (sock, 1024))))
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
                                             (case args of
                                                  ["stop", "daemon"] =>
                                                  (((Socket.close listen;
                                                     OS.FileSys.remove socket) handle OS.SysErr _ => ());
                                                   OS.Process.exit OS.Process.success)
                                                | _ =>
                                                  let
                                                      val success = (oneRun (rev args) handle DaemonExit => OS.Process.exit OS.Process.success)
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
                                                  end)
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

                         Settings.reset ();
                         MLton.GC.pack ();
                         loop ()
                     end
             in
                 OS.Process.atExit (fn () => OS.FileSys.remove socket);
                 Socket.bind (listen, UnixSock.toAddr socket);
                 Socket.listen (listen, 1);
                 loop ()
             end

fun oneCommandLine args =
    let
        val sock = UnixSock.Strm.socket ()

        fun wait () =
            let
                val v = Socket.recvVec (sock, 1024)
            in
                if Word8Vector.length v = 0 then
                    OS.Process.failure
                else
                    let
                        val s = MLton.CharVector.fromPoly (Vector.map (chr o Word8.toInt) (MLton.Word8Vector.toPoly v))
                        val last = Word8Vector.sub (v, Word8Vector.length v - 1)
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
             wait ())
        else
            (OS.FileSys.remove socket;
             raise OS.SysErr ("", NONE))
    end handle OS.SysErr _ => oneRun args handle DaemonExit => OS.Process.success
            
val () = (Globals.setResetTime ();
          case CommandLine.arguments () of
              ["daemon", "start"] => startDaemon ()
            | ["daemon", "restart"] =>
              (ignore (oneCommandLine ["daemon", "stop"]);
               startDaemon ())
            | args => OS.Process.exit (oneCommandLine args))
