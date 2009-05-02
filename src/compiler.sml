(* Copyright (c) 2008-2009, Adam Chlipala
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

structure Compiler :> COMPILER = struct 

structure UrwebLrVals = UrwebLrValsFn(structure Token = LrParser.Token)
structure Lex = UrwebLexFn(structure Tokens = UrwebLrVals.Tokens)
structure UrwebP = Join(structure ParserData = UrwebLrVals.ParserData
                         structure Lex = Lex
                         structure LrParser = LrParser)

type job = {
     prefix : string,
     database : string option,
     sources : string list,
     exe : string,
     sql : string option,
     debug : bool,
     profile : bool,
     timeout : int,
     ffi : string list,
     link : string list,
     headers : string list,
     clientToServer : Settings.ffi list,
     effectful : Settings.ffi list,
     clientOnly : Settings.ffi list,
     serverOnly : Settings.ffi list,
     jsFuncs : (Settings.ffi * string) list
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

fun check (tr : ('src, 'dst) transform) x = (ErrorMsg.resetErrors ();
                                             ignore (#func tr x))

fun run (tr : ('src, 'dst) transform) x = (ErrorMsg.resetErrors ();
                                           #func tr x)

fun runPrint (tr : ('src, 'dst) transform) input =
    (ErrorMsg.resetErrors ();
     case #func tr input of
         NONE => print "Failure\n"
       | SOME v =>
         (print "Success\n";
          Print.print (#print tr v);
          print "\n"))

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

fun p_job {prefix, database, exe, sql, sources, debug, profile,
           timeout, ffi, link, headers,
           clientToServer, effectful, clientOnly, serverOnly, jsFuncs} =
    let
        open Print.PD
        open Print

        fun p_ffi name = p_list_sep (box []) (fn (m, s) =>
                                                 box [string name, space, string m, string ".", string s, newline])
    in
        box [if debug then
                 box [string "DEBUG", newline]
             else
                 box [],
             if profile then
                 box [string "PROFILE", newline]
             else
                 box [],
             case database of
                 NONE => string "No database."
               | SOME db => string ("Database: " ^ db),
             newline,
             string "Exe: ",
             string exe,
             newline,
             case sql of
                 NONE => string "No SQL file."
               | SOME sql => string ("SQL fle: " ^ sql),
             newline,
             string "Timeout: ",
             string (Int.toString timeout),
             newline,
             p_list_sep (box []) (fn s => box [string "Ffi", space, string s, newline]) ffi,
             p_list_sep (box []) (fn s => box [string "Header", space, string s, newline]) headers,
             p_list_sep (box []) (fn s => box [string "Link", space, string s, newline]) link,
             p_ffi "ClientToServer" clientToServer,
             p_ffi "Effectful" effectful,
             p_ffi "ClientOnly" clientOnly,
             p_ffi "ServerOnly" serverOnly,
             p_list_sep (box []) (fn ((m, s), s') =>
                                     box [string "JsFunc", space, string m, string ".", string s,
                                          space, string "=", space, string s', newline]) jsFuncs,
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

                  fun relify fname =
                      OS.Path.concat (dir, fname)
                      handle OS.Path.Path => fname

                  val absDir = OS.Path.mkAbsolute {path = dir, relativeTo = OS.FileSys.getDir ()}

                  fun relifyA fname = OS.Path.mkAbsolute {path = fname, relativeTo = absDir}

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
                                                val fname = relify fname
                                            in
                                                fname :: acc
                                            end
                          in
                              readSources acc
                          end

                  val prefix = ref NONE
                  val database = ref NONE
                  val exe = ref NONE
                  val sql = ref NONE
                  val debug = ref false
                  val profile = ref false
                  val timeout = ref NONE
                  val ffi = ref []
                  val link = ref []
                  val headers = ref []
                  val clientToServer = ref []
                  val effectful = ref []
                  val clientOnly = ref []
                  val serverOnly = ref []
                  val jsFuncs = ref []

                  fun finish sources =
                      {prefix = Option.getOpt (!prefix, "/"),
                       database = !database,
                       exe = Option.getOpt (!exe, OS.Path.joinBaseExt {base = OS.Path.base filename,
                                                                       ext = SOME "exe"}),
                       sql = !sql,
                       debug = !debug,
                       profile = !profile,
                       timeout = Option.getOpt (!timeout, 60),
                       ffi = rev (!ffi),
                       link = rev (!link),
                       headers = rev (!headers),
                       clientToServer = rev (!clientToServer),
                       effectful = rev (!effectful),
                       clientOnly = rev (!clientOnly),
                       serverOnly = rev (!serverOnly),
                       jsFuncs = rev (!jsFuncs),
                       sources = sources}

                  fun read () =
                      case TextIO.inputLine inf of
                          NONE => finish []
                        | SOME "\n" => finish (readSources [])
                        | SOME line =>
                          let
                              val (cmd, arg) = Substring.splitl (fn x => not (Char.isSpace x)) (Substring.full line)
                              val cmd = Substring.string (trim cmd)
                              val arg = Substring.string (trim arg)

                              fun ffiS () =
                                  case String.fields (fn ch => ch = #".") arg of
                                      [m, x] => (m, x)
                                    | _ => (ErrorMsg.error (cmd ^ " argument not of the form Module.func");
                                            ("", ""))

                              fun ffiM () =
                                  case String.fields (fn ch => ch = #"=") arg of
                                      [f, s] =>
                                      (case String.fields (fn ch => ch = #".") f of
                                           [m, x] => ((m, x), s)
                                         | _ => (ErrorMsg.error (cmd ^ " argument not of the form Module.func=func'");
                                                 (("", ""), "")))
                                    | _ => (ErrorMsg.error (cmd ^ " argument not of the form Module.func=func'");
                                            (("", ""), ""))
                          in
                              case cmd of
                                  "prefix" =>
                                  (case !prefix of
                                       NONE => ()
                                     | SOME _ => ErrorMsg.error "Duplicate 'prefix' directive";
                                   prefix := SOME arg)
                                | "database" =>
                                  (case !database of
                                       NONE => ()
                                     | SOME _ => ErrorMsg.error "Duplicate 'database' directive";
                                   database := SOME arg)
                                | "exe" =>
                                  (case !exe of
                                       NONE => ()
                                     | SOME _ => ErrorMsg.error "Duplicate 'exe' directive";
                                   exe := SOME (relify arg))
                                | "sql" =>
                                  (case !sql of
                                       NONE => ()
                                     | SOME _ => ErrorMsg.error "Duplicate 'sql' directive";
                                   sql := SOME (relify arg))
                                | "debug" => debug := true
                                | "profile" => profile := true
                                | "timeout" =>
                                  (case !timeout of
                                       NONE => ()
                                     | SOME _ => ErrorMsg.error "Duplicate 'timeout' directive";
                                   timeout := SOME (valOf (Int.fromString arg)))
                                | "ffi" => ffi := relify arg :: !ffi
                                | "link" => link := relifyA arg :: !link
                                | "include" => headers := relifyA arg :: !headers
                                | "clientToServer" => clientToServer := ffiS () :: !clientToServer
                                | "effectful" => effectful := ffiS () :: !effectful
                                | "clientOnly" => clientOnly := ffiS () :: !clientOnly
                                | "serverOnly" => serverOnly := ffiS () :: !serverOnly
                                | "jsFunc" => jsFuncs := ffiM () :: !jsFuncs
                                | _ => ErrorMsg.error ("Unrecognized command '" ^ cmd ^ "'");
                              read ()
                          end

                  val job = read ()
              in
                  TextIO.closeIn inf;
                  Settings.setUrlPrefix (#prefix job);
                  Settings.setTimeout (#timeout job);
                  Settings.setHeaders (#headers job);
                  Settings.setClientToServer (#clientToServer job);
                  Settings.setEffectful (#effectful job);
                  Settings.setClientOnly (#clientOnly job);
                  Settings.setServerOnly (#serverOnly job);
                  Settings.setJsFuncs (#jsFuncs job);
                  job
              end,
    print = p_job
}

val toParseJob = transform parseUrp "parseJob"

fun capitalize "" = ""
  | capitalize s = str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

val parse = {
    func = fn {database, sources = fnames, ffi, ...} : job =>
              let
                  fun nameOf fname = capitalize (OS.Path.file fname)

                  fun parseFfi fname =
                      let
                          val mname = nameOf fname
                          val urs = OS.Path.joinBaseExt {base = fname, ext = SOME "urs"}

                          val loc = {file = urs,
                                     first = ErrorMsg.dummyPos,
                                     last = ErrorMsg.dummyPos}

                          val sgn = (Source.SgnConst (#func parseUrs urs), loc)
                      in
                          (Source.DFfiStr (mname, sgn), loc)
                      end

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

                  val dsFfi = map parseFfi ffi
                  val ds = map parseOne fnames
              in
                  let
                      val final = nameOf (List.last fnames)

                      val ds = dsFfi @ ds
                               @ [(Source.DExport (Source.StrVar final, ErrorMsg.dummySpan), ErrorMsg.dummySpan)]
                  in
                      case database of
                          NONE => ds
                        | SOME s => (Source.DDatabase s, ErrorMsg.dummySpan) :: ds
                  end handle Empty => ds
              end,
    print = SourcePrint.p_file
}

val toParse = transform parse "parse" o toParseJob

fun libFile s = OS.Path.joinDirFile {dir = Config.libUr,
                                     file = s}
fun clibFile s = OS.Path.joinDirFile {dir = Config.libC,
                                      file = s}

val elaborate = {
    func = fn file => let
                  val basis = #func parseUrs (libFile "basis.urs")
                  val topSgn = #func parseUrs (libFile "top.urs")
                  val topStr = #func parseUr (libFile "top.ur")
              in
                  Elaborate.elabFile basis topStr topSgn ElabEnv.empty file
              end,
    print = ElabPrint.p_file ElabEnv.empty
}

val toElaborate = transform elaborate "elaborate" o toParse

val unnest = {
    func = Unnest.unnest,
    print = ElabPrint.p_file ElabEnv.empty
}

val toUnnest = transform unnest "unnest" o toElaborate

val termination = {
    func = (fn file => (Termination.check file; file)),
    print = ElabPrint.p_file ElabEnv.empty
}

val toTermination = transform termination "termination" o toUnnest

val explify = {
    func = Explify.explify,
    print = ExplPrint.p_file ExplEnv.empty
}

val toExplify = transform explify "explify" o toUnnest

val corify = {
    func = Corify.corify,
    print = CorePrint.p_file CoreEnv.empty
}

val toCorify = transform corify "corify" o toExplify

(*val reduce_local = {
    func = ReduceLocal.reduce,
    print = CorePrint.p_file CoreEnv.empty
}

val toReduce_local = transform reduce_local "reduce_local" o toCorify*)

val especialize = {
    func = ESpecialize.specialize,
    print = CorePrint.p_file CoreEnv.empty
}

val toEspecialize = transform especialize "especialize" o toCorify

val core_untangle = {
    func = CoreUntangle.untangle,
    print = CorePrint.p_file CoreEnv.empty
}

val toCore_untangle = transform core_untangle "core_untangle" o toEspecialize

val shake = {
    func = Shake.shake,
    print = CorePrint.p_file CoreEnv.empty
}

val toShake1 = transform shake "shake1" o toCore_untangle

val rpcify = {
    func = Rpcify.frob,
    print = CorePrint.p_file CoreEnv.empty
}

val toRpcify = transform rpcify "rpcify" o toShake1

val toCore_untangle2 = transform core_untangle "core_untangle2" o toRpcify
val toShake2 = transform shake "shake2" o toCore_untangle2

val tag = {
    func = Tag.tag,
    print = CorePrint.p_file CoreEnv.empty
}

val toTag = transform tag "tag" o toCore_untangle2

val reduce = {
    func = Reduce.reduce,
    print = CorePrint.p_file CoreEnv.empty
}

val toReduce = transform reduce "reduce" o toTag

val unpoly = {
    func = Unpoly.unpoly,
    print = CorePrint.p_file CoreEnv.empty
}

val toUnpoly = transform unpoly "unpoly" o toReduce

val specialize = {
    func = Specialize.specialize,
    print = CorePrint.p_file CoreEnv.empty
}

val toSpecialize = transform specialize "specialize" o toUnpoly

val toShake3 = transform shake "shake3" o toSpecialize

val marshalcheck = {
    func = (fn file => (MarshalCheck.check file; file)),
    print = CorePrint.p_file CoreEnv.empty
}

val toMarshalcheck = transform marshalcheck "marshalcheck" o toShake3

val effectize = {
    func = Effective.effectize,
    print = CorePrint.p_file CoreEnv.empty
}

val toEffectize = transform effectize "effectize" o toMarshalcheck

val monoize = {
    func = Monoize.monoize CoreEnv.empty,
    print = MonoPrint.p_file MonoEnv.empty
}

val toMonoize = transform monoize "monoize" o toEffectize

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

val jscomp = {
    func = JsComp.process,
    print = MonoPrint.p_file MonoEnv.empty
}

val toJscomp = transform jscomp "jscomp" o toMono_opt2

val toMono_opt3 = transform mono_opt "mono_opt3" o toJscomp

val fuse = {
    func = Fuse.fuse,
    print = MonoPrint.p_file MonoEnv.empty
}

val toFuse = transform fuse "fuse" o toMono_opt3

val toUntangle2 = transform untangle "untangle2" o toFuse

val toMono_reduce2 = transform mono_reduce "mono_reduce2" o toUntangle2
val toMono_shake2 = transform mono_shake "mono_shake2" o toMono_reduce2

val pathcheck = {
    func = (fn file => (PathCheck.check file; file)),
    print = MonoPrint.p_file MonoEnv.empty
}

val toPathcheck = transform pathcheck "pathcheck" o toMono_shake2

val cjrize = {
    func = Cjrize.cjrize,
    print = CjrPrint.p_file CjrEnv.empty
}

val toCjrize = transform cjrize "cjrize" o toPathcheck

val scriptcheck = {
    func = ScriptCheck.classify,
    print = CjrPrint.p_file CjrEnv.empty
}

val toScriptcheck = transform scriptcheck "scriptcheck" o toCjrize

val prepare = {
    func = Prepare.prepare,
    print = CjrPrint.p_file CjrEnv.empty
}

val toPrepare = transform prepare "prepare" o toScriptcheck

val sqlify = {
    func = Cjrize.cjrize,
    print = CjrPrint.p_sql CjrEnv.empty
}

val toSqlify = transform sqlify "sqlify" o toMono_opt2

fun compileC {cname, oname, ename, libs, profile, debug, link = link'} =
    let
        val urweb_o = clibFile "urweb.o"
        val driver_o = clibFile "driver.o"

        val compile = "gcc " ^ Config.gccArgs ^ " -Wstrict-prototypes -Werror -O3 -I include -c " ^ cname ^ " -o " ^ oname
        val link = "gcc -Werror -O3 -lm -lmhash -pthread " ^ libs ^ " " ^ urweb_o ^ " " ^ oname ^ " " ^ driver_o ^ " -o " ^ ename

        val (compile, link) =
            if profile then
                (compile ^ " -pg", link ^ " -pg")
            else
                (compile, link)

        val (compile, link) =
            if debug then
                (compile ^ " -g", link ^ " -g")
            else
                (compile, link)

        val link = foldl (fn (s, link) => link ^ " " ^ s) link link'
    in
        if not (OS.Process.isSuccess (OS.Process.system compile)) then
            print "C compilation failed\n"
        else if not (OS.Process.isSuccess (OS.Process.system link)) then
            print "C linking failed\n"
        else
            ()
    end

fun compile job =
    case run toPrepare job of
        NONE => print "Ur compilation failed\n"
      | SOME file =>
        let
            val job = valOf (run (transform parseUrp "parseUrp") job)

            val (cname, oname, cleanup) =
                if #debug job then
                    ("/tmp/webapp.c", "/tmp/webapp.o", fn () => ())
                else
                    let
                        val dir = OS.FileSys.tmpName ()
                        val () = if OS.FileSys.access (dir, []) then
                                     OS.FileSys.remove dir
                                 else
                                     ()
                        val cname = OS.Path.joinDirFile {dir = dir, file = "webapp.c"}
                        val oname = OS.Path.joinDirFile {dir = dir, file = "webapp.o"}
                    in
                        OS.FileSys.mkDir dir;
                        (cname, oname,
                         fn () => (OS.FileSys.remove cname;
                                   OS.FileSys.remove oname;
                                   OS.FileSys.rmDir dir)
                            handle OS.SysErr _ => OS.FileSys.rmDir dir)
                    end
            val ename = #exe job
        in
            let
                val outf = TextIO.openOut cname
                val s = TextIOPP.openOut {dst = outf, wid = 80}

                val hasDb = List.exists (fn (Cjr.DDatabase _, _) => true | _ => false) (#1 file)
                val libs =
                    if hasDb then
                        "-lpq"
                    else
                        ""
            in
                Print.fprint s (CjrPrint.p_file CjrEnv.empty file);
		TextIO.output1 (outf, #"\n");
                TextIO.closeOut outf;

                case #sql job of
                    NONE => ()
                  | SOME sql =>
                    let
                        val outf = TextIO.openOut sql
                        val s = TextIOPP.openOut {dst = outf, wid = 80}
                    in
                        Print.fprint s (CjrPrint.p_sql CjrEnv.empty file);
                        TextIO.closeOut outf
                    end;

                compileC {cname = cname, oname = oname, ename = ename, libs = libs,
                          profile = #profile job, debug = #debug job, link = #link job};
                
                cleanup ()
            end
            handle ex => (((cleanup ()) handle _ => ()); raise ex)
        end

end
