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

(* Laconic/Web language parser *)

structure Compiler :> COMPILER = struct 

structure LacwebLrVals = LacwebLrValsFn(structure Token = LrParser.Token)
structure Lex = LacwebLexFn(structure Tokens = LacwebLrVals.Tokens)
structure LacwebP = Join(structure ParserData = LacwebLrVals.ParserData
                         structure Lex = Lex
                         structure LrParser = LrParser)

(* The main parsing routine *)
fun parse filename =
    let
        val () = (ErrorMsg.resetErrors ();
                  ErrorMsg.resetPositioning filename)
	val file = TextIO.openIn filename
	fun get _ = TextIO.input file
	fun parseerror (s, p1, p2) = ErrorMsg.errorAt' (p1, p2) s
	val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	val (absyn, _) = LacwebP.parse (30, lexer, parseerror, ())
    in
        TextIO.closeIn file;
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME absyn
    end
    handle LrParser.ParseError => NONE

fun elaborate env filename =
    case parse filename of
        NONE => NONE
      | SOME file =>
        let
            val out = Elaborate.elabFile env file
        in
            if ErrorMsg.anyErrors () then
                NONE
            else
                SOME out
        end

fun corify eenv cenv filename =
    case elaborate eenv filename of
        NONE => NONE
      | SOME (file, _) =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Corify.corify file)

fun reduce eenv cenv filename =
    case corify eenv cenv filename of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Reduce.reduce (Shake.shake file))

fun shake eenv cenv filename =
    case reduce eenv cenv filename of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Shake.shake file)

fun monoize eenv cenv filename =
    case shake eenv cenv filename of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Monoize.monoize cenv file)

fun cloconv eenv cenv filename =
    case monoize eenv cenv filename of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Cloconv.cloconv file)

fun cjrize eenv cenv filename =
    case cloconv eenv cenv filename of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Cjrize.cjrize file)

fun testParse filename =
    case parse filename of
        NONE => print "Failed\n"
      | SOME file =>
        (Print.print (SourcePrint.p_file file);
         print "\n")

fun testElaborate filename =
    (case elaborate ElabEnv.basis filename of
         NONE => print "Failed\n"
       | SOME (file, _) =>
         (Print.print (ElabPrint.p_file ElabEnv.basis file);
          print "\n"))
    handle ElabEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testCorify filename =
    (case corify ElabEnv.basis CoreEnv.basis filename of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.basis file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testReduce filename =
    (case reduce ElabEnv.basis CoreEnv.basis filename of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.basis file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testShake filename =
    (case shake ElabEnv.basis CoreEnv.basis filename of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.basis file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testMonoize filename =
    (case monoize ElabEnv.basis CoreEnv.basis filename of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (MonoPrint.p_file MonoEnv.basis file);
          print "\n"))
    handle MonoEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testCloconv filename =
    (case cloconv ElabEnv.basis CoreEnv.basis filename of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (FlatPrint.p_file FlatEnv.basis file);
          print "\n"))
    handle FlatEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testCjrize filename =
    (case cjrize ElabEnv.basis CoreEnv.basis filename of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CjrPrint.p_file CjrEnv.basis file);
          print "\n"))
    handle CjrEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun compile filename =
    case cjrize ElabEnv.basis CoreEnv.basis filename of
        NONE => ()
      | SOME file =>
        let
            val outf = TextIO.openOut "/tmp/lacweb.c"
            val s = TextIOPP.openOut {dst = outf, wid = 80}
        in
            Print.fprint s (CjrPrint.p_file CjrEnv.basis file);
            TextIO.closeOut outf
        end

end
