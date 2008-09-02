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

(* Ur/Web language parser *)

structure Compiler :> COMPILER = struct 

structure UrwebLrVals = UrwebLrValsFn(structure Token = LrParser.Token)
structure Lex = UrwebLexFn(structure Tokens = UrwebLrVals.Tokens)
structure UrwebP = Join(structure ParserData = UrwebLrVals.ParserData
                         structure Lex = Lex
                         structure LrParser = LrParser)

type job = {
     database : string option,
     sources : string list
}

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

fun op o (tr2 : ('b, 'c) transform, tr1 : ('a, 'b) transform) = {
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

val parseUrs =
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
	           val (absyn, _) = UrwebP.parse (30, lexer, parseerror, ())
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
val parseUr = {
    func = fn filename =>
              let
                  val () = (ErrorMsg.resetErrors ();
                            ErrorMsg.resetPositioning filename;
                            Lex.UserDeclarations.initialize ())
	          val file = TextIO.openIn filename
	          fun get _ = TextIO.input file
	          fun parseerror (s, p1, p2) = ErrorMsg.errorAt' (p1, p2) s
	          val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	          val (absyn, _) = UrwebP.parse (30, lexer, parseerror, ())
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

fun p_job {database, sources} =
    let
        open Print.PD
        open Print
    in
        box [case database of
                 NONE => string "No database."
               | SOME db => string ("Database: " ^ db),
             newline,
             string "Sources:",
             p_list string sources,
             newline]
    end

fun trim s =
    let
        val (_, s) = Substring.splitl Char.isSpace s
        val (s, _) = Substring.splitr Char.isSpace s
    in
        s
    end

val parseUrp = {
    func = fn filename =>
              let
                  val dir = OS.Path.dir filename
                  val inf = TextIO.openIn (OS.Path.joinBaseExt {base = filename, ext = SOME "urp"})

                  fun readSources acc =
                      case TextIO.inputLine inf of
                          NONE => rev acc
                        | SOME line =>
                          let
                              val acc = if CharVector.all Char.isSpace line then
                                            acc
                                        else
                                            let
                                                val fname = String.implode (List.filter (fn x => not (Char.isSpace x))
                                                                                        (String.explode line))
                                                val fname = OS.Path.concat (dir, fname)
                                                            handle OS.Path.Path => fname
                                            in
                                                fname :: acc
                                            end
                          in
                              readSources acc
                          end

                  fun read database =
                      case TextIO.inputLine inf of
                          NONE => {database = database, sources = []}
                        | SOME "\n" => {database = database, sources = readSources []}
                        | SOME line =>
                          let
                              val (cmd, arg) = Substring.splitl (fn x => not (Char.isSpace x)) (Substring.full line)
                              val cmd = Substring.string (trim cmd)
                              val arg = Substring.string (trim arg)
                          in
                              case cmd of
                                  "database" =>
                                  (case database of
                                       NONE => ()
                                     | SOME _ => ErrorMsg.error "Duplicate 'database' directive";
                                   read (SOME arg))
                                | _ => (ErrorMsg.error ("Unrecognized command '" ^ cmd ^ "'");
                                        read database)
                          end
              in
                  read NONE
                  before TextIO.closeIn inf
              end,
    print = p_job
}

val toParseJob = transform parseUrp "parseJob"

fun capitalize "" = ""
  | capitalize s = str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

val parse = {
    func = fn {database, sources = fnames} =>
              let
                  fun nameOf fname = capitalize (OS.Path.file fname)

                  fun parseOne fname =
                      let
                          val mname = nameOf fname
                          val ur = OS.Path.joinBaseExt {base = fname, ext = SOME "ur"}
                          val urs = OS.Path.joinBaseExt {base = fname, ext = SOME "urs"}

                          val sgnO =
                              if Posix.FileSys.access (urs, []) then
                                  SOME (Source.SgnConst (#func parseUrs urs),
                                        {file = urs,
                                         first = ErrorMsg.dummyPos,
                                         last = ErrorMsg.dummyPos})
                              else
                                  NONE

                          val loc = {file = ur,
                                     first = ErrorMsg.dummyPos,
                                     last = ErrorMsg.dummyPos}

                          val ds = #func parseUr ur
                      in
                          (Source.DStr (mname, sgnO, (Source.StrConst ds, loc)), loc)
                      end

                  val ds = map parseOne fnames
              in
                  let
                      val final = nameOf (List.last fnames)

                      val ds = ds @ [(Source.DExport (Source.StrVar final, ErrorMsg.dummySpan), ErrorMsg.dummySpan)]
                  in
                      case database of
                          NONE => ds
                        | SOME s => (Source.DDatabase s, ErrorMsg.dummySpan) :: ds
                  end handle Empty => ds
              end,
    print = SourcePrint.p_file
}

val toParse = transform parse "parse" o toParseJob

val elaborate = {
    func = fn file => let
                  val basis = #func parseUrs "lib/basis.urs"
              in
                  Elaborate.elabFile basis ElabEnv.empty file
              end,
    print = ElabPrint.p_file ElabEnv.empty
}

val toElaborate = transform elaborate "elaborate" o toParse

val explify = {
    func = Explify.explify,
    print = ExplPrint.p_file ExplEnv.empty
}

val toExplify = transform explify "explify" o toElaborate

val corify = {
    func = Corify.corify,
    print = CorePrint.p_file CoreEnv.empty
}

val toCorify = transform corify "corify" o toExplify

val shake = {
    func = Shake.shake,
    print = CorePrint.p_file CoreEnv.empty
}

val toShake1 = transform shake "shake1" o toCorify

val tag = {
    func = Tag.tag,
    print = CorePrint.p_file CoreEnv.empty
}

val toTag = transform tag "tag" o toShake1

val reduce = {
    func = Reduce.reduce,
    print = CorePrint.p_file CoreEnv.empty
}

val toReduce = transform reduce "reduce" o toTag

val specialize = {
    func = Specialize.specialize,
    print = CorePrint.p_file CoreEnv.empty
}

val toSpecialize = transform specialize "specialize" o toReduce

val toShake2 = transform shake "shake2" o toSpecialize

val monoize = {
    func = Monoize.monoize CoreEnv.empty,
    print = MonoPrint.p_file MonoEnv.empty
}

val toMonoize = transform monoize "monoize" o toShake2

val mono_opt = {
    func = MonoOpt.optimize,
    print = MonoPrint.p_file MonoEnv.empty
}

val toMono_opt1 = transform mono_opt "mono_opt1" o toMonoize

val untangle = {
    func = Untangle.untangle,
    print = MonoPrint.p_file MonoEnv.empty
}

val toUntangle = transform untangle "untangle" o toMono_opt1

val mono_reduce = {
    func = MonoReduce.reduce,
    print = MonoPrint.p_file MonoEnv.empty
}

val toMono_reduce = transform mono_reduce "mono_reduce" o toUntangle

val mono_shake = {
    func = MonoShake.shake,
    print = MonoPrint.p_file MonoEnv.empty
}

val toMono_shake = transform mono_shake "mono_shake1" o toMono_reduce

val toMono_opt2 = transform mono_opt "mono_opt2" o toMono_shake

val cjrize = {
    func = Cjrize.cjrize,
    print = CjrPrint.p_file CjrEnv.empty
}

val toCjrize = transform cjrize "cjrize" o toMono_opt2

fun compileC {cname, oname, ename} =
    let
        val compile = "gcc -O3 -I include -c " ^ cname ^ " -o " ^ oname
        val link = "gcc -pthread -O3 clib/urweb.o " ^ oname ^ " clib/driver.o -o " ^ ename
    in
        if not (OS.Process.isSuccess (OS.Process.system compile)) then
            print "C compilation failed\n"
        else if not (OS.Process.isSuccess (OS.Process.system link)) then
            print "C linking failed\n"
        else
            print "Success\n"
    end

fun compile job =
    case run toCjrize job of
        NONE => print "Ur compilation failed\n"
      | SOME file =>
        let
            val cname = "/tmp/urweb.c"
            val oname = "/tmp/urweb.o"
            val ename = "/tmp/webapp"

            val outf = TextIO.openOut cname
            val s = TextIOPP.openOut {dst = outf, wid = 80}
        in
            Print.fprint s (CjrPrint.p_file CjrEnv.empty file);
            TextIO.closeOut outf;

            compileC {cname = cname, oname = oname, ename = ename}
        end

end
