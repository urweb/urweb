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

structure Tutorial :> TUTORIAL = struct

fun readAll inf =
    let
        fun loop acc =
            case TextIO.inputLine inf of
                NONE => Substring.full (String.concat (rev acc))
              | SOME line => loop (line :: acc)
    in
        loop []
        before TextIO.closeIn inf
    end

val readAllFile = readAll o TextIO.openIn

fun fixupFile (fname, title) =
    let
        val source = readAllFile "/tmp/final.html"
        val outf = TextIO.openOut (OS.Path.mkAbsolute {relativeTo = OS.FileSys.getDir (),
                                                       path = OS.Path.joinBaseExt {base = OS.Path.base fname, ext = SOME "html"}})

        val (befor, after) = Substring.position "<title>" source

        fun proseLoop source =
            let
                val (befor, after) = Substring.splitl (fn ch => ch <> #"&") source
            in
                if Substring.isEmpty after then
                    TextIO.outputSubstr (outf, source)
                else if Substring.size after >= 4 andalso Substring.string (Substring.slice (after, 1, SOME 3)) = "lt;" then
                    (TextIO.outputSubstr (outf, befor);
                     TextIO.output (outf, "<");
                     proseLoop (Substring.slice (after, 4, NONE)))
                else if Substring.size after >= 4 andalso Substring.string (Substring.slice (after, 1, SOME 3)) = "gt;" then
                    (TextIO.outputSubstr (outf, befor);
                     TextIO.output (outf, ">");
                     proseLoop (Substring.slice (after, 4, NONE)))
                else if Substring.size after >= 5 andalso Substring.string (Substring.slice (after, 1, SOME 4)) = "amp;" then
                    (TextIO.outputSubstr (outf, befor);
                     TextIO.output (outf, "&");
                     proseLoop (Substring.slice (after, 5, NONE)))
                else
                    raise Fail "Unsupported HTML escape"
            end

        fun loop source =
            let
                val (befor, after) = Substring.position "<span class=\"comment-delimiter\">(* </span><span class=\"comment\">" source
            in
                if Substring.isEmpty after then
                   TextIO.outputSubstr (outf, source)
                else
                    let
                        val (befor', after) = Substring.position " </span><span class=\"comment-delimiter\">*)</span>"
                                                                 (Substring.slice (after, 64, NONE))
                    in
                        if Substring.isEmpty after then
                            TextIO.outputSubstr (outf, source)
                        else
                            (TextIO.outputSubstr (outf, befor);
                             TextIO.output (outf, "</pre>");
                             if Substring.size befor' >= 1 andalso Substring.sub (befor', 0) = #"*" then
                                 (TextIO.output (outf, "<h2>");
                                  proseLoop (Substring.slice (befor', 2, NONE));
                                  TextIO.output (outf, "</h2>"))
                             else
                                 (TextIO.output (outf, "<div class=\"prose\">");
                                  proseLoop befor';
                                  TextIO.output (outf, "</div>"));
                             TextIO.output (outf, "<pre>");
                             loop (Substring.slice (after, 49, NONE)))
                    end
            end
    in
        if Substring.isEmpty after then
            raise Fail ("Missing <title> for " ^ title)
        else
            (TextIO.outputSubstr (outf, befor);
             TextIO.output (outf, "<style type=\"text/css\">\n");
             TextIO.output (outf, "<!--\n");
             TextIO.output (outf, "\tdiv.prose {\n");
             TextIO.output (outf, "\t\tfont-family: Arial;\n");
             TextIO.output (outf, "\t\tbackground-color: #CCFFCC;\n");
             TextIO.output (outf, "\t\tborder-style: solid;\n");
             TextIO.output (outf, "\t\tpadding: 5px;\n");
             TextIO.output (outf, "\t\tfont-size: larger;\n");
             TextIO.output (outf, "\t}\n");
             TextIO.output (outf, "\th2 {\n");
             TextIO.output (outf, "\t\tfont-family: Arial;\n");
             TextIO.output (outf, "\t\tfont-size: 20pt;\n");
             TextIO.output (outf, "\t\tbackground-color: #99FF99;\n");
             TextIO.output (outf, "\t\tpadding: 5px;\n");
             TextIO.output (outf, "\t}\n");
             TextIO.output (outf, "\ta:link {\n");
             TextIO.output (outf, "\t\ttext-decoration: underline;\n");
             TextIO.output (outf, "\t\tcolor: blue;\n");
             TextIO.output (outf, "\t}\n");
             TextIO.output (outf, "\ta:visited {\n");
             TextIO.output (outf, "\t\ttext-decoration: underline;\n");
             TextIO.output (outf, "\t\tcolor: red;\n");
             TextIO.output (outf, "\t}\n");
             TextIO.output (outf, "-->\n");
             TextIO.output (outf, "</style>\n");
             TextIO.output (outf, "<title>");
             TextIO.output (outf, title);
             let
                 val (befor, after) = Substring.position "</title>" after
             in
                 if Substring.isEmpty after then
                     raise Fail ("Missing </title> for " ^ title)
                 else
                     let
                         val (befor, after) = Substring.position "<body>" after
                     in
                         if Substring.isEmpty after then
                             raise Fail ("Missing <body> for " ^ title)
                         else
                             (TextIO.outputSubstr (outf, befor);
                              TextIO.output (outf, "<body><h1>");
                              TextIO.output (outf, title);
                              TextIO.output (outf, "</h1>");
                              loop (Substring.slice (after, 6, NONE)))
                     end
             end;
             TextIO.closeOut outf)
    end

fun doUr fname =
    let
        val inf = TextIO.openIn fname

        val title = case TextIO.inputLine inf of
                        NONE => raise Fail ("No title comment at start of " ^ fname)
                      | SOME title => title

        val title = String.substring (title, 3, size title - 7)

        val eval = TextIO.openOut "/tmp/eval.ur"
        val gen = TextIO.openOut "/tmp/gen.ur"

        fun untilEnd source =
            let
                val (befor, after) = Substring.position "(* end *)" source
            in
                if Substring.isEmpty after then
                    (source, Substring.full "")
                else
                    (befor, Substring.slice (after, 9, NONE))
            end

        fun doDirectives (count, source) =
            let
                val safe = String.translate (fn #"<" => "&lt;"
                                              | #"&" => "&amp;"
                                              | #"{" => "&#123;"
                                              | #"(" => "&#40;"
                                              | #"\n" => "&#40;*NL*)\n"
                                              | #" " => "&#40;*NL*) "
                                              | ch => str ch) o Substring.string

                val (befor, after) = Substring.position "(* begin " source

                fun default () = (TextIO.outputSubstr (eval, source);
                                  TextIO.output (gen, safe source))
            in
                if Substring.isEmpty after then
                    default ()
                else
                    let
                        val (command, after) = Substring.splitl (not o Char.isSpace) (Substring.slice (after, 9, NONE))
                    in
                        if Substring.isEmpty after then
                            default ()
                        else
                            let
                                val (_, rest) = Substring.position "*)" after
                            in
                                if Substring.isEmpty rest then
                                    default ()
                                else
                                    let
                                        val (arg, source) = untilEnd (Substring.slice (rest, 3, NONE))
                                        val () = (TextIO.outputSubstr (eval, befor);
                                                  TextIO.output (gen, safe befor))
                                        val (count, skip) =
                                            case Substring.string command of
                                                "hide" => (TextIO.outputSubstr (eval, arg);
                                                           (count, true))
                                              | "eval" => (TextIO.output (eval, "val _eval");
                                                           TextIO.output (eval, Int.toString count);
                                                           TextIO.output (eval, " = ");
                                                           TextIO.outputSubstr (eval, arg);
                                                           TextIO.output (eval, "\n\n");

                                                           TextIO.output (gen, safe arg);
                                                           TextIO.output (gen, "== {[_eval");
                                                           TextIO.output (gen, Int.toString count);
                                                           TextIO.output (gen, "]}");

                                                           (count + 1, false))
                                              | s => raise Fail ("Unknown tutorial directive: " ^ s)
                                    in
                                        doDirectives (count, if skip then
                                                                 #2 (Substring.splitl Char.isSpace source)
                                                             else
                                                                 source)
                                    end
                            end
                    end
            end
    in
        doDirectives (0, readAll inf);
        TextIO.closeOut gen;

        TextIO.output (eval, "\n\nfun main () : transaction page =\nreturn <xml><body>");
        TextIO.outputSubstr (eval, readAllFile "/tmp/gen.ur");
        TextIO.output (eval, "</body></xml>");
        TextIO.closeOut eval;

        if Compiler.compile "/tmp/eval" then
            let
                val proc = Unix.execute ("/bin/sh", ["-c", "/tmp/eval.exe /main"])
                val inf = Unix.textInstreamOf proc
                val s = readAll inf
                val _ = Unix.reap proc

                val (befor, after) = Substring.position "<sc>" s
            in
                if Substring.isEmpty after then
                    print ("Bad output for " ^ fname ^ "! [1]\n")
                else
                    let
                        val after = Substring.slice (after, 4, NONE)
                        val (befor, after) = Substring.position "</body>" after
                    in
                        if Substring.isEmpty after then
                            print ("Bad output for " ^ fname ^ "! [2]\n")
                        else
                            let
                                val outf = TextIO.openOut "/tmp/final.ur"

                                fun eatNls source =
                                    let
                                        val (befor, after) = Substring.position "(*NL*)" source
                                    in
                                        if Substring.isEmpty after then
                                            TextIO.outputSubstr (outf, source)
                                        else
                                            (TextIO.outputSubstr (outf, befor);
                                             eatNls (Substring.slice (after, 6, NONE)))
                                    end

                                val cmd = "emacs --eval \"(progn "
                                          ^ "(global-font-lock-mode t) "
                                          ^ "(add-to-list 'load-path \\\""
                                          ^ !Settings.configSitelisp
                                          ^ "/\\\") "
                                          ^ "(load \\\"urweb-mode-startup\\\") "
                                          ^ "(urweb-mode) "
                                          ^ "(find-file \\\"/tmp/final2.ur\\\") "
                                          ^ "(switch-to-buffer (htmlize-buffer)) "
                                          ^ "(write-file \\\"/tmp/final.html\\\") "
                                          ^ "(kill-emacs))\""
                            in
                                eatNls befor;
                                TextIO.closeOut outf;
                                ignore (OS.Process.system "sed -e 's/&lt;/</g;s/&amp;/\\&/g' </tmp/final.ur >/tmp/final2.ur");
                                ignore (OS.Process.system cmd);
                                fixupFile (fname, title)
                            end
                    end
            end
        else
            ()
    end

fun make dirname =
    let
        val dir = OS.FileSys.openDir dirname

        fun doDir () =
            case OS.FileSys.readDir dir of
                NONE => OS.FileSys.closeDir dir
              | SOME fname =>
                (if OS.Path.ext fname = SOME "ur" then
                     doUr (OS.Path.joinDirFile {dir = dirname, file = fname})
                 else
                     ();
                 doDir ())
    in
        Settings.setProtocol "static";
        doDir ()
    end

end
