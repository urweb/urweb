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

structure Demo :> DEMO = struct

fun make {prefix, dirname, guided} =
    let
        val prose = OS.Path.joinDirFile {dir = dirname,
                                         file = "prose"}
        val inf = TextIO.openIn prose

        val outDir = OS.Path.concat (dirname, "out")

        val () = if OS.FileSys.access (outDir, []) then
                     ()
                 else
                     OS.FileSys.mkDir outDir

        val fname = OS.Path.joinDirFile {dir = outDir,
                                         file = "index.html"}

        val out = TextIO.openOut fname
        val () = (TextIO.output (out, "<frameset cols=\"10%,90%\">\n");
                  TextIO.output (out, "<frame src=\"demos.html\">\n");
                  TextIO.output (out, "<frame src=\"intro.html\" name=\"staging\">\n");
                  TextIO.output (out, "</frameset>\n");
                  TextIO.closeOut out)

        val fname = OS.Path.joinDirFile {dir = outDir,
                                         file = "demos.html"}

        val demosOut = TextIO.openOut fname
        val () = (TextIO.output (demosOut, "<html><body>\n\n");
                  TextIO.output (demosOut, "<li> <a target=\"staging\" href=\"intro.html\">Intro</a></li>\n\n"))

        val fname = OS.Path.joinDirFile {dir = dirname,
                                         file = "demo.urs"}
        val ursOut = TextIO.openOut fname
        val () = (TextIO.output (ursOut, "val main : unit -> transaction page\n");
                  TextIO.closeOut ursOut)

        val fname = OS.Path.joinDirFile {dir = dirname,
                                         file = "demo.ur"}
        val urOut = TextIO.openOut fname
        val () = TextIO.output (urOut, "fun main () = return <xml><body>\n")

        fun mergeWith f (o1, o2) =
            case (o1, o2) of
                (NONE, _) => o2
              | (_, NONE) => o1
              | (SOME v1, SOME v2) => SOME (f (v1, v2))

        fun combiner (combined : Compiler.job, urp : Compiler.job) = {
            prefix = prefix,
            database = mergeWith (fn (v1, v2) =>
                                     if v1 = v2 then
                                         v1
                                     else
                                         raise Fail "Different demos want to use different database strings")
                                 (#database combined, #database urp),
            sources = foldl (fn (file, files) =>
                                if List.exists (fn x => x = file) files then
                                    files
                                else
                                    files @ [file])
                            (#sources combined) (#sources urp),
            exe = OS.Path.joinDirFile {dir = dirname,
                                       file = "demo.exe"},
            sql = SOME (OS.Path.joinDirFile {dir = dirname,
                                             file = "demo.sql"}),
            debug = false,
            timeout = Int.max (#timeout combined, #timeout urp),
            profile = false,
            ffi = [],
            link = [],
            headers = [],
            clientToServer = [],
            effectful = [],
            clientOnly = [],
            serverOnly = [],
            jsFuncs = []
        }

        val parse = Compiler.run (Compiler.transform Compiler.parseUrp "Demo parseUrp")

        fun capitalize "" = ""
          | capitalize s = str (Char.toUpper (String.sub (s, 0)))
                           ^ String.extract (s, 1, NONE)

        fun startUrp urp =
            let
                val base = OS.Path.base urp
                val name = capitalize base

                val () = (TextIO.output (demosOut, "<li> <a target=\"staging\" href=\"");
                          TextIO.output (demosOut, base);
                          TextIO.output (demosOut, ".html\">");
                          TextIO.output (demosOut, name);
                          TextIO.output (demosOut, "</a></li>\n"))

                val () = (TextIO.output (urOut, "  <li> <a link={");
                          TextIO.output (urOut, name);
                          TextIO.output (urOut, ".main ()}>");
                          TextIO.output (urOut, name);
                          TextIO.output (urOut, "</a></li>\n"))

                val urp_file = OS.Path.joinDirFile {dir = dirname,
                                                    file = urp}

                val out = OS.Path.joinBaseExt {base = base,
                                               ext = SOME "html"}
                val out = OS.Path.joinDirFile {dir = outDir,
                                               file = out}
                val out = TextIO.openOut out

                val () = (TextIO.output (out, "<frameset rows=\"");
                          TextIO.output (out, if guided then
                                                  "*,100"
                                              else
                                                  "50%,*");
                          TextIO.output (out, "\">\n");
                          TextIO.output (out, "<frame src=\"");
                          TextIO.output (out, prefix);
                          TextIO.output (out, "/");
                          TextIO.output (out, name);
                          TextIO.output (out, "/main\" name=\"showcase\">\n");
                          TextIO.output (out, "<frame src=\"");
                          TextIO.output (out, base);
                          TextIO.output (out, ".desc.html\">\n");
                          TextIO.output (out, "</frameset>\n");
                          TextIO.closeOut out)
                val () = TextIO.closeOut out

                val out = OS.Path.joinBaseExt {base = base,
                                               ext = SOME "desc"}
                val out = OS.Path.joinBaseExt {base = out,
                                               ext = SOME "html"}
                val out = TextIO.openOut (OS.Path.joinDirFile {dir = outDir,
                                                               file = out})
            in
                case parse (OS.Path.base urp_file) of
                    NONE => raise Fail ("Can't parse " ^ urp_file)
                  | SOME urpData =>
                    (TextIO.output (out, "<html><head>\n<title>");
                     TextIO.output (out, name);
                     TextIO.output (out, "</title>\n</head><body>\n\n<h1>");
                     TextIO.output (out, name);
                     TextIO.output (out, "</h1>\n\n<center>[ <a target=\"showcase\" href=\"");
                     TextIO.output (out, prefix);
                     TextIO.output (out, "/");
                     TextIO.output (out, name);
                     TextIO.output (out, "/main\">Application</a>");
                     TextIO.output (out, " | <a target=\"showcase\" href=\"");
                     TextIO.output (out, urp);
                     TextIO.output (out, ".html\"><tt>");
                     TextIO.output (out, urp);
                     TextIO.output (out, "</tt></a>");
                     app (fn file =>
                             let
                                 fun ifEx s =
                                     let
                                         val src = OS.Path.joinBaseExt {base = file,
                                                                        ext = SOME s}
                                         val src' = OS.Path.file src
                                     in
                                         if OS.FileSys.access (src, []) then
                                             (TextIO.output (out, " | <a target=\"showcase\" href=\"");
                                              TextIO.output (out, src');
                                              TextIO.output (out, ".html\"><tt>");
                                              TextIO.output (out, src');
                                              TextIO.output (out, "</tt></a>"))
                                         else
                                             ()
                                     end
                             in
                                 ifEx "urs";
                                 ifEx "ur"
                             end) (#sources urpData);
                     TextIO.output (out, " ]</center>\n\n");

                     (urpData, out))
            end

        fun endUrp out =
            (TextIO.output (out, "\n</body></html>\n");
             TextIO.closeOut out)

        fun readUrp (combined, out) =
            let
                fun finished () = endUrp out

                fun readUrp' () =
                    case TextIO.inputLine inf of
                        NONE => (finished ();
                                 combined)
                      | SOME line =>
                        if String.isSuffix ".urp\n" line then
                            let
                                val urp = String.substring (line, 0, size line - 1)
                                val (urpData, out) = startUrp urp
                            in
                                finished ();

                                readUrp (combiner (combined, urpData),
                                         out)
                            end
                        else
                            (TextIO.output (out, line);
                             readUrp' ())
            in
                readUrp' ()
            end

        val indexFile = OS.Path.joinDirFile {dir = outDir,
                                             file = "intro.html"}

        val out = TextIO.openOut indexFile
        val () = TextIO.output (out, "<html><head>\n<title>Ur/Web Demo</title>\n</head><body>\n\n")

        fun readIndex () =
            let
                fun finished () = (TextIO.output (out, "\n</body></html>\n");
                                   TextIO.closeOut out)
            in
                case TextIO.inputLine inf of
                    NONE => (finished ();
                             NONE)
                  | SOME line =>
                    if String.isSuffix ".urp\n" line then
                        let
                            val urp = String.substring (line, 0, size line - 1)
                            val (urpData, out) = startUrp urp
                        in
                            finished ();
                            
                            SOME (readUrp (urpData,
                                           out))
                        end
                    else
                        (TextIO.output (out, line);
                         readIndex ())
            end

        fun prettyPrint () =
            let
                val dir = Posix.FileSys.opendir dirname

                fun loop () =
                    case Posix.FileSys.readdir dir of
                        NONE => Posix.FileSys.closedir dir
                      | SOME file =>
                        let
                            fun doit f =
                                f (OS.Path.joinDirFile {dir = dirname,
                                                        file = file},
                                   OS.Path.mkAbsolute
                                       {relativeTo = OS.FileSys.getDir (),
                                        path = OS.Path.joinDirFile {dir = outDir,
                                                                    file = OS.Path.joinBaseExt {base = file,
                                                                                                ext = SOME "html"}}})

                            fun highlight () =
                                doit (fn (src, html) =>
                                         let
                                             val dirty =
                                                 let
                                                     val srcSt = Posix.FileSys.stat src
                                                     val htmlSt = Posix.FileSys.stat html
                                                 in
                                                     Time.> (Posix.FileSys.ST.mtime srcSt,
                                                             Posix.FileSys.ST.mtime htmlSt)
                                                 end handle OS.SysErr _ => true

                                             val cmd = "emacs --eval \"(progn "
                                                       ^ "(global-font-lock-mode t) "
                                                       ^ "(add-to-list 'load-path \\\""
                                                       ^ Config.sitelisp
                                                       ^ "/\\\") "
                                                       ^ "(load \\\"urweb-mode-startup\\\") "
                                                       ^ "(urweb-mode) "
                                                       ^ "(find-file \\\""
                                                       ^ src
                                                       ^ "\\\") "
                                                       ^ "(switch-to-buffer (htmlize-buffer)) "
                                                       ^ "(write-file \\\""
                                                       ^ html
                                                       ^ "\\\") "
                                                       ^ "(kill-emacs))\""
                                         in
                                             if dirty then
                                                 (print (">>> " ^ cmd ^ "\n");
                                                  ignore (OS.Process.system cmd))
                                             else
                                                 ()
                                         end)
                        in
                            if OS.Path.base file = "demo" then
                                ()
                            else case OS.Path.ext file of
                                     SOME "urp" =>
                                     doit (fn (src, html) =>
                                              let
                                                  val inf = TextIO.openIn src
                                                  val out = TextIO.openOut html

                                                  fun loop () =
                                                      case TextIO.inputLine inf of
                                                          NONE => ()
                                                        | SOME line => (TextIO.output (out, line);
                                                                        loop ())
                                              in
                                                  TextIO.output (out, "<html><body>\n\n<pre>");
                                                  loop ();
                                                  TextIO.output (out, "</pre>\n\n</body></html>");

                                                  TextIO.closeIn inf;
                                                  TextIO.closeOut out
                                              end)
                                   | SOME "urs" => highlight ()
                                   | SOME "ur" => highlight ()
                                   | _ => ();
                            loop ()
                        end
            in
                loop ()
            end
    in
        case readIndex () of
            NONE => raise Fail "No demo applications!"
          | SOME combined =>
            let
                val () = (TextIO.output (urOut, "</body></xml>\n");
                          TextIO.closeOut urOut)

                val fname = OS.Path.joinDirFile {dir = dirname,
                                                 file = "demo.urp"}
                val outf = TextIO.openOut fname
            in
                Option.app (fn db => (TextIO.output (outf, "database ");
                                      TextIO.output (outf, db);
                                      TextIO.output (outf, "\n")))
                           (#database combined);
                TextIO.output (outf, "sql demo.sql\n");
                TextIO.output (outf, "prefix ");
                TextIO.output (outf, prefix);
                TextIO.output (outf, "\n");
                TextIO.output (outf, "\n");

                app (fn s =>
                        let
                            val s = OS.Path.mkAbsolute {relativeTo = OS.FileSys.getDir (),
                                                        path = s}
                        in
                            TextIO.output (outf, s);
                            TextIO.output (outf, "\n")
                        end)
                    (#sources combined);
                TextIO.output (outf, "\n");
                TextIO.output (outf, "demo\n");

                TextIO.closeOut outf;

                Compiler.compile (OS.Path.base fname)
            end;

        TextIO.output (demosOut, "\n</body></html>\n");
        TextIO.closeOut demosOut;

        prettyPrint ()
    end

end
