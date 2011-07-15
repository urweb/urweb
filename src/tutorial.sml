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

fun readAll' inf =
    let
        fun loop acc =
            case TextIO.inputLine inf of
                NONE => Substring.full (String.concat (rev acc))
              | SOME line => loop (line :: acc)
    in
        loop []
        before TextIO.closeIn inf
    end

fun readAll fname = readAll' (TextIO.openIn fname)

fun doUr fname =
    let
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
        doDirectives (0, readAll fname);
        TextIO.closeOut gen;

        TextIO.output (eval, "\n\nfun main () : transaction page =\nreturn <xml><body>");
        TextIO.outputSubstr (eval, readAll "/tmp/gen.ur");
        TextIO.output (eval, "</body></xml>");
        TextIO.closeOut eval;

        if Compiler.compile "/tmp/eval" then
            let
                val proc = Unix.execute ("/bin/sh", ["-c", "/tmp/eval.exe /main"])
                val inf = Unix.textInstreamOf proc
                val s = readAll' inf
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
                                          ^ Config.sitelisp
                                          ^ "/\\\") "
                                          ^ "(load \\\"urweb-mode-startup\\\") "
                                          ^ "(urweb-mode) "
                                          ^ "(find-file \\\"/tmp/final.ur\\\") "
                                          ^ "(switch-to-buffer (htmlize-buffer)) "
                                          ^ "(write-file \\\""
                                          ^ OS.Path.mkAbsolute {relativeTo = OS.FileSys.getDir (),
                                                                path = OS.Path.joinBaseExt {base = OS.Path.base fname, ext = SOME "html"}}
                                          ^ "\\\") "
                                          ^ "(kill-emacs))\""
                            in
                                eatNls befor;
                                TextIO.closeOut outf;
                                ignore (OS.Process.system cmd)
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
