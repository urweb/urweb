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

type job = string list

type ('src, 'dst) phase = {
     func : 'src -> 'dst,
     print : 'dst -> Print.PD.pp_desc
}

type pmap = (string * Time.time) list

type ('src, 'dst) transform = {
     func : 'src -> 'dst option,
     print : 'dst -> Print.PD.pp_desc,
     time : 'src * pmap -> 'dst option * pmap
}

fun transform (ph : ('src, 'dst) phase) name = {
    func = fn input => let
                  val v = #func ph input
              in
                  if ErrorMsg.anyErrors () then
                      NONE
                  else
                      SOME v
              end,
    print = #print ph,
    time = fn (input, pmap) => let
                  val befor = Time.now ()
                  val v = #func ph input
                  val elapsed = Time.- (Time.now (), befor)
              in
                  (if ErrorMsg.anyErrors () then
                       NONE
                   else
                       SOME v,
                   (name, elapsed) :: pmap)
              end
}

fun op o (tr1 : ('a, 'b) transform, tr2 : ('b, 'c) transform) = {
    func = fn input => case #func tr1 input of
                           NONE => NONE
                         | SOME v => #func tr2 v,
    print = #print tr2,
    time = fn (input, pmap) => let
                  val (ro, pmap) = #time tr1 (input, pmap)
              in
                  case ro of
                      NONE => (NONE, pmap)
                    | SOME v => #time tr2 (v, pmap)
              end
}

fun run (tr : ('src, 'dst) transform) = #func tr

fun runPrint (tr : ('src, 'dst) transform) input =
    case #func tr input of
        NONE => print "Failure\n"
      | SOME v =>
        (print "Success\n";
         Print.print (#print tr v);
         print "\n")

fun time (tr : ('src, 'dst) transform) input =
    let
        val (_, pmap) = #time tr (input, [])
    in
        app (fn (name, time) =>
                print (name ^ ": " ^ LargeReal.toString (Time.toReal time) ^ "\n")) (rev pmap);
        print ("TOTAL: " ^ LargeReal.toString (Time.toReal (foldl Time.+ Time.zeroTime (map #2 pmap))) ^ "\n");
        print "\n"
    end

fun timePrint (tr : ('src, 'dst) transform) input =
    let
        val (ro, pmap) = #time tr (input, [])
    in
        app (fn (name, time) =>
                print (name ^ ": " ^ LargeReal.toString (Time.toReal time) ^ "\n")) (rev pmap);
        print ("TOTAL: " ^ LargeReal.toString (Time.toReal (foldl Time.+ Time.zeroTime (map #2 pmap))) ^ "\n");
        print "\n";
        case ro of
            NONE => print "Failure\n"
          | SOME v =>
            (print "Success\n";
             Print.print (#print tr v);
             print "\n")
    end

val parseLig =
    {func = fn filename => let
                   val fname = OS.FileSys.tmpName ()
                   val outf = TextIO.openOut fname
                   val () = TextIO.output (outf, "sig\n")
                   val inf = TextIO.openIn filename
                   fun loop () =
                       case TextIO.inputLine inf of
                           NONE => ()
                         | SOME line => (TextIO.output (outf, line);
                                         loop ())
                   val () = loop ()
                   val () = TextIO.closeIn inf
                   val () = TextIO.closeOut outf

                   val () = (ErrorMsg.resetErrors ();
                             ErrorMsg.resetPositioning filename;
                             Lex.UserDeclarations.initialize ())
	           val file = TextIO.openIn fname
	           fun get _ = TextIO.input file
	           fun parseerror (s, p1, p2) = ErrorMsg.errorAt' (p1, p2) s
	           val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	           val (absyn, _) = LacwebP.parse (30, lexer, parseerror, ())
               in
                   TextIO.closeIn file;
                   case absyn of
                       [(Source.DSgn ("?", (Source.SgnConst sgis, _)), _)] => sgis
                     | _ => (ErrorMsg.errorAt {file = filename,
                                               first = {line = 0,
                                                        char = 0},
                                               last = {line = 0,
                                                       char = 0}} "Not a signature";
                             [])
               end
               handle LrParser.ParseError => [],
     print = Print.p_list_sep Print.PD.newline SourcePrint.p_sgn_item}

(* The main parsing routine *)
val parseLac = {
    func = fn filename =>
              let
                  val () = (ErrorMsg.resetErrors ();
                            ErrorMsg.resetPositioning filename;
                            Lex.UserDeclarations.initialize ())
	          val file = TextIO.openIn filename
	          fun get _ = TextIO.input file
	          fun parseerror (s, p1, p2) = ErrorMsg.errorAt' (p1, p2) s
	          val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	          val (absyn, _) = LacwebP.parse (30, lexer, parseerror, ())
              in
                  TextIO.closeIn file;
                  case absyn of
                      [(Source.DSgn ("?", _), _)] =>
                      (ErrorMsg.errorAt {file = filename,
                                         first = {line = 0,
                                                  char = 0},
                                         last = {line = 0,
                                                 char = 0}} "File starts with 'sig'";
                       [])
                    | _ => absyn
              end
              handle LrParser.ParseError => [],
     print = SourcePrint.p_file}    

fun capitalize "" = ""
  | capitalize s = str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

val parse = {
    func = fn fnames =>
              let
                  fun nameOf fname = capitalize (OS.Path.file fname)

                  fun parseOne fname =
                      let
                          val mname = nameOf fname
                          val lac = OS.Path.joinBaseExt {base = fname, ext = SOME "lac"}
                          val lig = OS.Path.joinBaseExt {base = fname, ext = SOME "lig"}

                          val sgnO =
                              if Posix.FileSys.access (lig, []) then
                                  SOME (Source.SgnConst (#func parseLig lig),
                                        {file = lig,
                                         first = ErrorMsg.dummyPos,
                                         last = ErrorMsg.dummyPos})
                              else
                                  NONE

                          val loc = {file = lac,
                                     first = ErrorMsg.dummyPos,
                                     last = ErrorMsg.dummyPos}

                          val ds = #func parseLac lac
                      in
                          (Source.DStr (mname, sgnO, (Source.StrConst ds, loc)), loc)
                      end

                  val ds = map parseOne fnames
              in
                  let
                      val final = nameOf (List.last fnames)
                  in
                      ds @ [(Source.DExport (Source.StrVar final, ErrorMsg.dummySpan), ErrorMsg.dummySpan)]
                  end handle Empty => ds
              end,
    print = SourcePrint.p_file
}

val toParse = transform parse "parse"

val elaborate = {
    func = fn file => let
                  val basis = #func parseLig "lib/basis.lig"
              in
                  Elaborate.elabFile basis ElabEnv.empty file
              end,
    print = ElabPrint.p_file ElabEnv.empty
}

val toElaborate = toParse o transform elaborate "elaborate"

val explify = {
    func = Explify.explify,
    print = ExplPrint.p_file ExplEnv.empty
}

val toExplify = toElaborate o transform explify "explify"

val corify = {
    func = Corify.corify,
    print = CorePrint.p_file CoreEnv.empty
}

val toCorify = toExplify o transform corify "corify"

(*fun shake' job =
    case corify job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Shake.shake file)

fun tag job =
    case shake' job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Tag.tag file)

fun reduce job =
    case tag job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Reduce.reduce file)

fun specialize job =
    case reduce job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Specialize.specialize file)

fun shake job =
    case specialize job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Shake.shake file)

fun monoize job =
    case shake job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Monoize.monoize CoreEnv.empty file)

fun mono_opt' job =
    case monoize job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (MonoOpt.optimize file)

fun untangle job =
    case mono_opt' job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Untangle.untangle file)

fun mono_reduce job =
    case untangle job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (MonoReduce.reduce file)

fun mono_shake job =
    case mono_reduce job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (MonoShake.shake file)

fun mono_opt job =
    case mono_shake job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (MonoOpt.optimize file)

fun cjrize job =
    case mono_opt job of
        NONE => NONE
      | SOME file =>
        if ErrorMsg.anyErrors () then
            NONE
        else
            SOME (Cjrize.cjrize file)

fun testParse job =
    case parse job of
        NONE => print "Failed\n"
      | SOME file =>
        (Print.print (SourcePrint.p_file file);
         print "\n")

fun testElaborate job =
    (case elaborate job of
         NONE => print "Failed\n"
       | SOME file =>
         (print "Succeeded\n";
          Print.print (ElabPrint.p_file ElabEnv.empty file);
          print "\n"))
    handle ElabEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testExplify job =
    (case explify job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (ExplPrint.p_file ExplEnv.empty file);
          print "\n"))
    handle ExplEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testCorify job =
    (case corify job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.empty file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testShake' job =
    (case shake' job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.empty file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testReduce job =
    (case reduce job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.empty file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testSpecialize job =
    (case specialize job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.empty file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testTag job =
    (case tag job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.empty file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testShake job =
    (case shake job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CorePrint.p_file CoreEnv.empty file);
          print "\n"))
    handle CoreEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testMonoize job =
    (case monoize job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (MonoPrint.p_file MonoEnv.empty file);
          print "\n"))
    handle MonoEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testMono_opt' job =
    (case mono_opt' job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (MonoPrint.p_file MonoEnv.empty file);
          print "\n"))
    handle MonoEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testUntangle job =
    (case untangle job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (MonoPrint.p_file MonoEnv.empty file);
          print "\n"))
    handle MonoEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testMono_reduce job =
    (case mono_reduce job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (MonoPrint.p_file MonoEnv.empty file);
          print "\n"))
    handle MonoEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testMono_shake job =
    (case mono_shake job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (MonoPrint.p_file MonoEnv.empty file);
          print "\n"))
    handle MonoEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testMono_opt job =
    (case mono_opt job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (MonoPrint.p_file MonoEnv.empty file);
          print "\n"))
    handle MonoEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")

fun testCjrize job =
    (case cjrize job of
         NONE => print "Failed\n"
       | SOME file =>
         (Print.print (CjrPrint.p_file CjrEnv.empty file);
          print "\n"))
    handle CjrEnv.UnboundNamed n =>
           print ("Unbound named " ^ Int.toString n ^ "\n")*)

fun compileC {cname, oname, ename} =
    let
        val compile = "gcc -O3 -I include -c " ^ cname ^ " -o " ^ oname
        val link = "gcc -pthread -O3 clib/lacweb.o " ^ oname ^ " clib/driver.o -o " ^ ename
    in
        if not (OS.Process.isSuccess (OS.Process.system compile)) then
            print "C compilation failed\n"
        else if not (OS.Process.isSuccess (OS.Process.system link)) then
            print "C linking failed\n"
        else
            print "Success\n"
    end

(*fun compile job =
    case cjrize job of
        NONE => print "Laconic compilation failed\n"
      | SOME file =>
        if ErrorMsg.anyErrors () then
            print "Laconic compilation failed\n"
        else
            let
                val cname = "/tmp/lacweb.c"
                val oname = "/tmp/lacweb.o"
                val ename = "/tmp/webapp"

                val outf = TextIO.openOut cname
                val s = TextIOPP.openOut {dst = outf, wid = 80}
            in
                Print.fprint s (CjrPrint.p_file CjrEnv.empty file);
                TextIO.closeOut outf;

                compileC {cname = cname, oname = oname, ename = ename}
            end*)

end
