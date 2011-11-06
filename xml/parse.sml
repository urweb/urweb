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

(* Building SML code from XML entity tables *)

fun main () =
    let
        fun doFile fname =
            let
                val inf = TextIO.openIn fname

                fun loop () =
                    case TextIO.inputLine inf of
                        NONE => TextIO.closeIn inf
                      | SOME line =>
                        if String.isPrefix "<!ENTITY " line then
                            case String.tokens (fn ch => Char.isSpace ch orelse ch = #">") line of
                                "<!ENTITY" :: ent :: exp :: _ =>
                                let
                                    val exp = if String.isPrefix "\"&#" exp andalso String.isSuffix ";\"" exp then
                                                  let
                                                      val middle = String.substring (exp, 3, size exp - 5)
                                                  in
                                                      if CharVector.all Char.isDigit middle then
                                                          middle
                                                      else if String.isPrefix "38;#" middle then
                                                          String.extract (middle, 4, NONE)
                                                      else
                                                          raise Fail "Bad entity expression [1]"
                                                  end
                                              else
                                                  raise Fail "Bad entity expansion [2]"
                                in
                                    print ("\t\t(\"" ^ ent ^ "\", " ^ exp ^ ") ::\n");
                                    loop ()
                                end
                              | _ => raise Fail "Bad ENTITY line"
                        else
                            loop ()
            in
                loop ()
            end
    in
        print "structure Entities = struct\n";
        print "\tval all =\n";
        doFile "xml/xhtml-lat1.ent";
        doFile "xml/xhtml-special.ent";
        doFile "xml/xhtml-symbol.ent";
        print "\t[]\n";
        print "end\n"
    end

val () = main ()
