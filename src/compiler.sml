(* Copyright (c) 2008-2010, Adam Chlipala
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
     scripts : string list,
     clientToServer : Settings.ffi list,
     effectful : Settings.ffi list,
     benignEffectful : Settings.ffi list,
     clientOnly : Settings.ffi list,
     serverOnly : Settings.ffi list,
     jsFuncs : (Settings.ffi * string) list,
     rewrites : Settings.rewrite list,
     filterUrl : Settings.rule list,
     filterMime : Settings.rule list,
     filterRequest : Settings.rule list,
     filterResponse : Settings.rule list,
     protocol : string option,
     dbms : string option,
     sigFile : string option,
     safeGets : string list,
     onError : (string * string list * string) option,
     minHeap : int
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

val debug = ref false
val doIflow = ref false

fun transform (ph : ('src, 'dst) phase) name = {
    func = fn input => let
                  val () = if !debug then
                               print ("Starting " ^ name ^ "....\n")
                           else
                               ()
                  val v = #func ph input
              in
                  if !debug then
                      print ("Finished " ^ name ^ ".\n")
                  else
                      ();
                  if ErrorMsg.anyErrors () then
                      NONE
                  else
                      SOME v
              end,
    print = #print ph,
    time = fn (input, pmap) => let
                  val () = if !debug then
                               print ("Starting " ^ name ^ "....\n")
                           else
                               ()
                  val befor = Time.now ()
                  val v = #func ph input
                  val elapsed = Time.- (Time.now (), befor)
              in
                  if !debug then
                      print ("Finished " ^ name ^ ".\n")
                  else
                      ();
                  (if ErrorMsg.anyErrors () then
                       NONE
                   else
                       SOME v,
                   (name, elapsed) :: pmap)
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

fun runPrintToFile (tr : ('src, 'dst) transform) input fname =
    (ErrorMsg.resetErrors ();
     case #func tr input of
         NONE => print "Failure\n"
       | SOME v =>
         let
             val outf = TextIO.openOut fname
             val str = Print.openOut {dst = outf, wid = 80}
         in
             print "Success\n";
             Print.fprint str (#print tr v);
             Print.PD.PPS.closeStream str;
             TextIO.closeOut outf
         end)

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

fun runPrintCoreFuncs (tr : ('src, Core.file) transform) input =
    (ErrorMsg.resetErrors ();
     case #func tr input of
         NONE => print "Failure\n"
       | SOME file =>
         (print "Success\n";
          app (fn (d, _) =>
                  case d of
                      Core.DVal (x, _, t, _, _) => Print.preface(x, CorePrint.p_con CoreEnv.empty t)
                    | Core.DValRec xts => app (fn (x, _, t, _, _) => Print.preface(x, CorePrint.p_con CoreEnv.empty t)) xts
                    | _ => ()) file))

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

fun p_job ({prefix, database, exe, sql, sources, debug, profile,
            timeout, ffi, link, headers, scripts,
            clientToServer, effectful, benignEffectful, clientOnly, serverOnly, jsFuncs, ...} : job) =
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
             p_list_sep (box []) (fn s => box [string "Script", space, string s, newline]) scripts,
             p_list_sep (box []) (fn s => box [string "Link", space, string s, newline]) link,
             p_ffi "ClientToServer" clientToServer,
             p_ffi "Effectful" effectful,
             p_ffi "BenignEffectful" benignEffectful,
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

val trimS = Substring.string o trim o Substring.full

structure M = BinaryMapFn(struct
                          type ord_key = string
                          val compare = String.compare
                          end)

val pathmap = ref (M.insert (M.empty, "", Config.libUr))

fun addPath (k, v) = pathmap := M.insert (!pathmap, k, v)

fun capitalize "" = ""
  | capitalize s = str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

fun institutionalizeJob (job : job) =
    (Settings.setDebug (#debug job);
     Settings.setUrlPrefix (#prefix job);
     Settings.setTimeout (#timeout job);
     Settings.setHeaders (#headers job);
     Settings.setScripts (#scripts job);
     Settings.setClientToServer (#clientToServer job);
     Settings.setEffectful (#effectful job);
     Settings.setBenignEffectful (#benignEffectful job);
     Settings.setClientOnly (#clientOnly job);
     Settings.setServerOnly (#serverOnly job);
     Settings.setJsFuncs (#jsFuncs job);
     Settings.setRewriteRules (#rewrites job);
     Settings.setUrlRules (#filterUrl job);
     Settings.setMimeRules (#filterMime job);
     Settings.setRequestHeaderRules (#filterRequest job);
     Settings.setResponseHeaderRules (#filterResponse job);
     Option.app Settings.setProtocol (#protocol job);
     Option.app Settings.setDbms (#dbms job);
     Settings.setSafeGets (#safeGets job);
     Settings.setOnError (#onError job);
     Settings.setMinHeap (#minHeap job);
     Settings.setSigFile (#sigFile job))

fun inputCommentableLine inf =
    Option.map (fn s =>
                   let
                       val s = #1 (Substring.splitl (fn ch => ch <> #"#") (Substring.full s))
                       val s = #1 (Substring.splitr (not o Char.isSpace) s)
                   in
                       Substring.string (if Substring.size s > 0 andalso Char.isSpace (Substring.sub (s, Substring.size s - 1)) then
                                             if Substring.size s > 1 andalso Char.isSpace (Substring.sub (s, Substring.size s - 2)) then
                                                 Substring.trimr 2 s
                                             else
                                                 Substring.trimr 1 s
                                         else
                                             s)
                   end) (TextIO.inputLine inf)

fun parseUrp' accLibs fname =
    if not (Posix.FileSys.access (fname ^ ".urp", []) orelse Posix.FileSys.access (fname ^ "/lib.urp", []))
       andalso Posix.FileSys.access (fname ^ ".ur", []) then
        let
            val job = {prefix = "/",
                       database = NONE,
                       sources = [fname],
                       exe = fname ^ ".exe",
                       sql = NONE,
                       debug = Settings.getDebug (),
                       profile = false,
                       timeout = 60,
                       ffi = [],
                       link = [],
                       headers = [],
                       scripts = [],
                       clientToServer = [],
                       effectful = [],
                       benignEffectful = [],
                       clientOnly = [],
                       serverOnly = [],
                       jsFuncs = [],
                       rewrites = [{pkind = Settings.Any,
                                    kind = Settings.Prefix,
                                    from = capitalize (OS.Path.file fname) ^ "/", to = ""}],
                       filterUrl = [],
                       filterMime = [],
                       filterRequest = [],
                       filterResponse = [],
                       protocol = NONE,
                       dbms = NONE,
                       sigFile = NONE,
                       safeGets = [],
                       onError = NONE,
                       minHeap = 0}
        in
            institutionalizeJob job;
            {Job = job, Libs = []}
        end
    else
        let
            val pathmap = ref (!pathmap)
            val bigLibs = ref []

            fun pu filename =
                let
                    val filename = OS.Path.mkAbsolute {path = filename, relativeTo = OS.FileSys.getDir ()}

                    val dir = OS.Path.dir filename
                    fun opener () = TextIO.openIn (OS.Path.joinBaseExt {base = filename, ext = SOME "urp"})

                    val inf = opener ()

                    fun hasSpaceLine () =
                        case inputCommentableLine inf of
                            NONE => false
                          | SOME s => s = "debug" orelse s = "profile"
                                      orelse CharVector.exists (fn ch => ch = #" " orelse ch = #"\t") s orelse hasSpaceLine ()

                    val hasBlankLine = hasSpaceLine ()

                    val inf = (TextIO.closeIn inf; opener ())

                    fun pathify fname =
                        if size fname > 0 andalso String.sub (fname, 0) = #"$" then
                            let
                                val fname' = Substring.extract (fname, 1, NONE)
                                val (befor, after) = Substring.splitl (fn ch => ch <> #"/") fname'
                            in
                                case M.find (!pathmap, Substring.string befor) of
                                    NONE => fname
                                  | SOME rep => rep ^ Substring.string after
                            end
                        else
                            fname

                    fun relify fname =
                        let
                            val fname = pathify fname
                        in
                            OS.Path.concat (dir, fname)
                            handle OS.Path.Path => fname
                        end

                    fun libify path =
                        (if Posix.FileSys.access (path ^ ".urp", []) then
                             path
                         else
                             path ^ "/lib")
                        handle SysErr => path

                    fun libify' path =
                        (if Posix.FileSys.access (relify path ^ ".urp", []) then
                             path
                         else
                             path ^ "/lib")
                        handle SysErr => path

                    val absDir = OS.Path.mkAbsolute {path = dir, relativeTo = OS.FileSys.getDir ()}

                    fun relifyA fname =
                        OS.Path.mkAbsolute {path = pathify fname, relativeTo = absDir}

                    fun readSources acc =
                        case inputCommentableLine inf of
                            NONE => rev acc
                          | SOME line =>
                            let
                                val acc = if CharVector.all Char.isSpace line then
                                              acc
                                          else
                                              let
                                                  val fname = String.implode (List.filter (fn x => not (Char.isSpace x))
                                                                                          (String.explode line))
                                                  val fname = relifyA fname
                                              in
                                                  fname :: acc
                                              end
                            in
                                readSources acc
                            end

                    val prefix = ref (case Settings.getUrlPrefix () of "/" => NONE | s => SOME s)
                    val database = ref (Settings.getDbstring ())
                    val exe = ref (Settings.getExe ())
                    val sql = ref (Settings.getSql ())
                    val debug = ref (Settings.getDebug ())
                    val profile = ref false
                    val timeout = ref NONE
                    val ffi = ref []
                    val link = ref []
                    val headers = ref []
                    val scripts = ref []
                    val clientToServer = ref []
                    val effectful = ref []
                    val benignEffectful = ref []
                    val clientOnly = ref []
                    val serverOnly = ref []
                    val jsFuncs = ref []
                    val rewrites = ref []
                    val url = ref []
                    val mime = ref []
                    val request = ref []
                    val response = ref []
                    val libs = ref []
                    val protocol = ref NONE
                    val dbms = ref NONE
                    val sigFile = ref (Settings.getSigFile ())
                    val safeGets = ref []
                    val onError = ref NONE
                    val minHeap = ref 0

                    fun finish sources =
                        let
                            val job = {
                                prefix = Option.getOpt (!prefix, "/"),
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
                                scripts = rev (!scripts),
                                clientToServer = rev (!clientToServer),
                                effectful = rev (!effectful),
                                benignEffectful = rev (!benignEffectful),
                                clientOnly = rev (!clientOnly),
                                serverOnly = rev (!serverOnly),
                                jsFuncs = rev (!jsFuncs),
                                rewrites = rev (!rewrites),
                                filterUrl = rev (!url),
                                filterMime = rev (!mime),
                                filterRequest = rev (!request),
                                filterResponse = rev (!response),
                                sources = sources,
                                protocol = !protocol,
                                dbms = !dbms,
                                sigFile = !sigFile,
                                safeGets = rev (!safeGets),
                                onError = !onError,
                                minHeap = !minHeap
                            }

                            fun mergeO f (old, new) =
                                case (old, new) of
                                    (NONE, _) => new
                                  | (_, NONE) => old
                                  | (SOME v1, SOME v2) => SOME (f (v1, v2))

                            fun same desc = mergeO (fn (x : string, y) =>
                                                       (if x = y then
                                                            ()
                                                        else
                                                            ErrorMsg.error ("Multiple "
                                                                            ^ desc ^ " values that don't agree");
                                                        x))

                            fun merge (old : job, new : job) = {
                                prefix = #prefix old,
                                database = mergeO (fn (old, _) => old) (#database old, #database new),
                                exe = #exe old,
                                sql = #sql old,
                                debug = #debug old orelse #debug new,
                                profile = #profile old orelse #profile new,
                                timeout = #timeout old,
                                ffi = #ffi old @ #ffi new,
                                link = #link old @ #link new,
                                headers = #headers old @ #headers new,
                                scripts = #scripts old @ #scripts new,
                                clientToServer = #clientToServer old @ #clientToServer new,
                                effectful = #effectful old @ #effectful new,
                                benignEffectful = #benignEffectful old @ #benignEffectful new,
                                clientOnly = #clientOnly old @ #clientOnly new,
                                serverOnly = #serverOnly old @ #serverOnly new,
                                jsFuncs = #jsFuncs old @ #jsFuncs new,
                                rewrites = #rewrites old @ #rewrites new,
                                filterUrl = #filterUrl old @ #filterUrl new,
                                filterMime = #filterMime old @ #filterMime new,
                                filterRequest = #filterRequest old @ #filterRequest new,
                                filterResponse = #filterResponse old @ #filterResponse new,
                                sources = #sources new
                                          @ List.filter (fn s => List.all (fn s' => s' <> s) (#sources new))
                                                        (#sources old),
                                protocol = mergeO #2 (#protocol old, #protocol new),
                                dbms = mergeO #2 (#dbms old, #dbms new),
                                sigFile = mergeO #2 (#sigFile old, #sigFile new),
                                safeGets = #safeGets old @ #safeGets new,
                                onError = mergeO #2 (#onError old, #onError new),
                                minHeap = Int.max (#minHeap old, #minHeap new)
                            }
                        in
                            if accLibs then
                                foldr (fn (job', job) => merge (job, job')) job (!libs)
                            else
                                job
                        end

                    fun parsePkind s =
                        case s of
                            "all" => Settings.Any
                          | "url" => Settings.Url
                          | "table" => Settings.Table
                          | "sequence" => Settings.Sequence
                          | "view" => Settings.View
                          | "relation" => Settings.Relation
                          | "cookie" => Settings.Cookie
                          | "style" => Settings.Style
                          | _ => (ErrorMsg.error "Bad path kind spec";
                                  Settings.Any)

                    fun parseFrom s =
                        if size s > 1 andalso String.sub (s, size s - 2) = #"/" andalso String.sub (s, size s - 1) = #"*" then
                            (Settings.Prefix, String.substring (s, 0, size s - 1))
                        else
                            (Settings.Exact, s)

                    fun parseFkind s =
                        case s of
                            "url" => url
                          | "mime" => mime
                          | "requestHeader" => request
                          | "responseHeader" => response
                          | _ => (ErrorMsg.error "Bad filter kind";
                                  url)

                    fun parsePattern s =
                        if size s > 0 andalso String.sub (s, size s - 1) = #"*" then
                            (Settings.Prefix, String.substring (s, 0, size s - 1))
                        else
                            (Settings.Exact, s)

                    fun read () =
                        case inputCommentableLine inf of
                            NONE => finish []
                          | SOME "" => finish (readSources [])
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
                                        let
                                            val f = trimS f
                                            val s = trimS s
                                        in
                                            case String.fields (fn ch => ch = #".") f of
                                                [m, x] => ((m, x), s)
                                              | _ => (ErrorMsg.error (cmd ^ " argument not of the form Module.func=func'");
                                                      (("", ""), ""))
                                        end
                                      | _ => (ErrorMsg.error (cmd ^ " argument not of the form Module.func=func'");
                                              (("", ""), ""))
                            in
                                case cmd of
                                    "prefix" => prefix := SOME arg
                                  | "database" =>
                                    (case !database of
                                         NONE => database := SOME arg
                                       | SOME _ => ())
                                  | "dbms" =>
                                    (case !dbms of
                                         NONE => dbms := SOME arg
                                       | SOME _ => ())
                                  | "sigfile" =>
                                    (case !sigFile of
                                         NONE => sigFile := SOME arg
                                       | SOME _ => ())
                                  | "exe" =>
                                    (case !exe of
                                         NONE => exe := SOME (relify arg)
                                       | SOME _ => ())
                                  | "sql" =>
                                    (case !sql of
                                         NONE => sql := SOME (relify arg)
                                       | SOME _ => ())
                                  | "debug" => debug := true
                                  | "profile" => profile := true
                                  | "timeout" =>
                                    (case !timeout of
                                         NONE => ()
                                       | SOME _ => ErrorMsg.error "Duplicate 'timeout' directive";
                                     timeout := SOME (valOf (Int.fromString arg)))
                                  | "ffi" => ffi := relify arg :: !ffi
                                  | "link" => let
                                        val arg = if size arg >= 1
                                                     andalso String.sub (arg, 0) = #"-" then
                                                      arg
                                                  else
                                                      relifyA arg
                                    in
                                        link := arg :: !link
                                    end
                                  | "include" => headers := relifyA arg :: !headers
                                  | "script" => scripts := arg :: !scripts
                                  | "clientToServer" => clientToServer := ffiS () :: !clientToServer
                                  | "safeGet" => safeGets := arg :: !safeGets
                                  | "effectful" => effectful := ffiS () :: !effectful
                                  | "benignEffectful" => benignEffectful := ffiS () :: !benignEffectful
                                  | "clientOnly" => clientOnly := ffiS () :: !clientOnly
                                  | "serverOnly" => serverOnly := ffiS () :: !serverOnly
                                  | "jsFunc" => jsFuncs := ffiM () :: !jsFuncs
                                  | "rewrite" =>
                                    let
                                        fun doit (pkind, from, to) =
                                            let
                                                val pkind = parsePkind pkind
                                                val (kind, from) = parseFrom from
                                            in
                                                rewrites := {pkind = pkind, kind = kind, from = from, to = to} :: !rewrites
                                            end
                                    in
                                        case String.tokens Char.isSpace arg of
                                            [pkind, from, to] => doit (pkind, from, to)
                                          | [pkind, from] => doit (pkind, from, "")
                                          | _ => ErrorMsg.error "Bad 'rewrite' syntax"
                                    end
                                  | "allow" =>
                                    (case String.tokens Char.isSpace arg of
                                         [fkind, pattern] =>
                                         let
                                             val fkind = parseFkind fkind
                                             val (kind, pattern) = parsePattern pattern
                                         in
                                             fkind := {action = Settings.Allow, kind = kind, pattern = pattern} :: !fkind
                                         end
                                       | _ => ErrorMsg.error "Bad 'allow' syntax")
                                  | "deny" =>
                                    (case String.tokens Char.isSpace arg of
                                         [fkind, pattern] =>
                                         let
                                             val fkind = parseFkind fkind
                                             val (kind, pattern) = parsePattern pattern
                                         in
                                             fkind := {action = Settings.Deny, kind = kind, pattern = pattern} :: !fkind
                                         end
                                       | _ => ErrorMsg.error "Bad 'deny' syntax")
                                  | "library" => if accLibs then
                                                     libs := pu (libify (relify arg)) :: !libs
                                                 else
                                                     bigLibs := libify' arg :: !bigLibs
                                  | "path" =>
                                    (case String.fields (fn ch => ch = #"=") arg of
                                         [n, v] => ((pathmap := M.insert (!pathmap, n, OS.Path.mkAbsolute {path = v, relativeTo = dir}))
                                                     handle OS.Path.Path => ErrorMsg.error "Invalid 'path' directory argument")
                                       | _ => ErrorMsg.error "path argument not of the form name=value'")
                                  | "onError" =>
                                    (case String.fields (fn ch => ch = #".") arg of
                                         m1 :: (fs as _ :: _) =>
                                         onError := SOME (m1, List.take (fs, length fs - 1), List.last fs)
                                       | _ => ErrorMsg.error "invalid 'onError' argument")
                                  | "limit" =>
                                    (case String.fields Char.isSpace arg of
                                         [class, num] =>
                                         (case Int.fromString num of
                                              NONE => ErrorMsg.error ("invalid limit number '" ^ num ^ "'")
                                            | SOME n =>
                                              if n < 0 then
                                                  ErrorMsg.error ("invalid limit number '" ^ num ^ "'")
                                              else
                                                  Settings.addLimit (class, n))
                                       | _ => ErrorMsg.error "invalid 'limit' arguments")
                                  | "minHeap" =>
                                    (case Int.fromString arg of
                                         NONE => ErrorMsg.error ("invalid min heap '" ^ arg ^ "'")
                                       | SOME n => minHeap := n)
                                  | "alwaysInline" => Settings.addAlwaysInline arg
                                  | "noXsrfProtection" => Settings.addNoXsrfProtection arg

                                  | _ => ErrorMsg.error ("Unrecognized command '" ^ cmd ^ "'");
                                read ()
                            end

                    val job = if hasBlankLine then
                                  read ()
                              else
                                  finish (readSources [])
                in
                    TextIO.closeIn inf;
                    institutionalizeJob job;
                    job
                end
        in
            {Job = pu fname, Libs = !bigLibs}
        end

fun p_job' {Job = j, Libs = _ : string list} = p_job j

val parseUrp = {
    func = #Job o parseUrp' true,
    print = p_job
}

val parseUrp' = {
    func = parseUrp' false,
    print = p_job'
}

val toParseJob = transform parseUrp "parseJob"
val toParseJob' = transform parseUrp' "parseJob'"

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

structure SM = BinaryMapFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

val moduleRoots = ref ([] : (string * string) list)
fun addModuleRoot (k, v) = moduleRoots := (k, v) :: !moduleRoots

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

val parse = {
    func = fn {database, sources = fnames, ffi, onError, ...} : job =>
              let
                  val mrs = !moduleRoots

                  val anyErrors = ref false
                  fun checkErrors () = anyErrors := (!anyErrors orelse ErrorMsg.anyErrors ())
                  fun nameOf fname =
                      let
                          val fname = OS.Path.file fname
                          val fst = if size fname = 0 then #"!" else String.sub (fname, 0)
                      in
                          if not (Char.isAlpha fst) then
                              ErrorMsg.error ("Filename doesn't start with letter: " ^ fname)
                          else if CharVector.exists (fn ch => not (Char.isAlphaNum ch) andalso ch <> #"_") fname then
                              ErrorMsg.error ("Filename contains a character that isn't alphanumeric or underscore: " ^ fname)
                          else
                              ();
                          capitalize fname
                      end

                  fun parseFfi fname =
                      let
                          val mname = nameOf fname
                          val urs = OS.Path.joinBaseExt {base = fname, ext = SOME "urs"}

                          val loc = {file = urs,
                                     first = ErrorMsg.dummyPos,
                                     last = ErrorMsg.dummyPos}

                          val sgn = (Source.SgnConst (#func parseUrs urs), loc)
                      in
                          checkErrors ();
                          (Source.DFfiStr (mname, sgn), loc)
                      end

                  val defed = ref SS.empty
                  val fulls = ref SS.empty

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
                                  before checkErrors ()
                              else
                                  NONE

                          val loc = {file = ur,
                                     first = ErrorMsg.dummyPos,
                                     last = ErrorMsg.dummyPos}

                          val ds = #func parseUr ur
                          val d = (Source.DStr (mname, sgnO, (Source.StrConst ds, loc)), loc)

                          val fname = OS.Path.mkCanonical fname
                          val d = case List.find (fn (root, name) =>
                                                     String.isPrefix (root ^ "/") fname) mrs of
                                      NONE => d
                                    | SOME (root, name) =>
                                      let
                                          val fname = String.extract (fname, size root + 1, NONE)
                                          val pieces = name :: String.tokens (fn ch => ch = #"/") fname
                                          val pieces = List.filter (fn s => size s > 0
                                                                            andalso Char.isAlpha (String.sub (s, 0)))
                                                                   pieces
                                          val pieces = map capitalize pieces
                                          val full = String.concatWith "." pieces

                                          fun makeD prefix pieces =
                                              case pieces of
                                                  [] => (ErrorMsg.error "Empty module path";
                                                         (Source.DStyle "Boo", loc))
                                                | [_] => d
                                                | piece :: pieces =>
                                                  let
                                                      val this = case prefix of
                                                                     "" => piece
                                                                   | _ => prefix ^ "." ^ piece
                                                      val old = SS.member (!defed, this)

                                                      fun notThere (ch, s) =
                                                          Substring.isEmpty (#2 (Substring.splitl
                                                                                     (fn ch' => ch' <> ch) s))

                                                      fun simOpen () =
                                                          SS.foldl (fn (full, ds) =>
                                                                       if String.isPrefix (this ^ ".") full
                                                                          andalso notThere (#".",
                                                                                            Substring.extract (full,
                                                                                                               size
                                                                                                                   this + 1,
                                                                                                               NONE)) then
                                                                           let
                                                                               val parts = String.tokens
                                                                                           (fn ch => ch = #".") full

                                                                               val part = List.last parts

                                                                               val imp = if length parts >= 2 then
                                                                                             (Source.StrProj
                                                                                                  ((Source.StrVar
                                                                                                        (List.nth (parts,
                                                                                                                   length
                                                                                                                       parts
                                                                                                                       - 2)),
                                                                                                    loc),
                                                                                                   part), loc)
                                                                                         else
                                                                                             (Source.StrVar part, loc)
                                                                           in
                                                                               (Source.DStr (part, NONE, imp),
                                                                                loc) :: ds
                                                                           end
                                                                       else
                                                                           ds) [] (!fulls)
                                                  in
                                                      defed := SS.add (!defed, this);
                                                      (Source.DStr (piece, NONE,
                                                                    (Source.StrConst (if old then
                                                                                          simOpen ()
                                                                                          @ [makeD this pieces]
                                                                                      else
                                                                                          [makeD this pieces]), loc)),
                                                       loc)
                                                  end
                                      in
                                          if SS.member (!fulls, full) then
                                              ErrorMsg.error ("Rooted module " ^ full ^ " has multiple versions.")
                                          else
                                              ();
                                          
                                          makeD "" pieces
                                          before ignore (foldl (fn (new, path) =>
                                                                   let
                                                                       val new' = case path of
                                                                                      "" => new
                                                                                    | _ => path ^ "." ^ new
                                                                   in
                                                                       fulls := SS.add (!fulls, new');
                                                                       new'
                                                                   end) "" pieces)
                                      end
                      in
                          checkErrors ();
                          d
                      end

                  val dsFfi = map parseFfi ffi
                  val ds = map parseOne fnames
                  val loc = ErrorMsg.dummySpan
              in
                  if !anyErrors then
                      ErrorMsg.error "Parse failure"
                  else
                      ();

                  let
                      val final = List.last fnames
                      val final = case List.find (fn (root, name) =>
                                                     String.isPrefix (root ^ "/") final) mrs of
                                      NONE => (Source.StrVar (nameOf final), loc)
                                    | SOME (root, name) =>
                                      let
                                          val m = (Source.StrVar name, loc)
                                          val final = String.extract (final, size root + 1, NONE)
                                          val fields = String.fields (fn ch => ch = #"/") final
                                          val fields = List.filter (fn s => size s = 0
                                                                            orelse not (Char.isDigit (String.sub (s, 0))))
                                                                   fields
                                      in
                                          foldl (fn (x, m) => (Source.StrProj (m, capitalize x), loc))
                                                m fields
                                      end

                      val ds = dsFfi @ ds
                               @ [(Source.DExport final, loc)]

                      val ds = case database of
                                   NONE => ds
                                 | SOME s => (Source.DDatabase s, loc) :: ds

                      val ds = case onError of
                                   NONE => ds
                                 | SOME v => ds @ [(Source.DOnError v, loc)]
                  in
                      ds
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

val core_untangle = {
    func = CoreUntangle.untangle,
    print = CorePrint.p_file CoreEnv.empty
}

val toCore_untangle = transform core_untangle "core_untangle" o toCorify

val shake = {
    func = Shake.shake,
    print = CorePrint.p_file CoreEnv.empty
}

val toShake1 = transform shake "shake1" o toCore_untangle

val toEspecialize1' = transform especialize "especialize1'" o toShake1
val toShake1' = transform shake "shake1'" o toEspecialize1'

val rpcify = {
    func = Rpcify.frob,
    print = CorePrint.p_file CoreEnv.empty
}

val toRpcify = transform rpcify "rpcify" o toShake1'

val toCore_untangle2 = transform core_untangle "core_untangle2" o toRpcify
val toShake2 = transform shake "shake2" o toCore_untangle2

val toEspecialize1 = transform especialize "especialize1" o toShake2

val toCore_untangle3 = transform core_untangle "core_untangle3" o toEspecialize1
val toShake3 = transform shake "shake3" o toCore_untangle3

val tag = {
    func = Tag.tag,
    print = CorePrint.p_file CoreEnv.empty
}

val toTag = transform tag "tag" o toShake3

val reduce = {
    func = Reduce.reduce,
    print = CorePrint.p_file CoreEnv.empty
}

val toReduce = transform reduce "reduce" o toTag

val toShakey = transform shake "shakey" o toReduce

val unpoly = {
    func = Unpoly.unpoly,
    print = CorePrint.p_file CoreEnv.empty
}

val toUnpoly = transform unpoly "unpoly" o toShakey

val specialize = {
    func = Specialize.specialize,
    print = CorePrint.p_file CoreEnv.empty
}

val toSpecialize = transform specialize "specialize" o toUnpoly

val toShake4 = transform shake "shake4" o toSpecialize

val toEspecialize2 = transform especialize "especialize2" o toShake4
val toShake4' = transform shake "shake4'" o toEspecialize2
val toUnpoly2 = transform unpoly "unpoly2" o toShake4'
val toSpecialize2 = transform specialize "specialize2" o toUnpoly2
val toShake4'' = transform shake "shake4'" o toSpecialize2
val toEspecialize3 = transform especialize "especialize3" o toShake4''

val toReduce2 = transform reduce "reduce2" o toEspecialize3

val toShake5 = transform shake "shake5" o toReduce2

val marshalcheck = {
    func = (fn file => (MarshalCheck.check file; file)),
    print = CorePrint.p_file CoreEnv.empty
}

val toMarshalcheck = transform marshalcheck "marshalcheck" o toShake5

val effectize = {
    func = Effective.effectize,
    print = CorePrint.p_file CoreEnv.empty
}

val toEffectize = transform effectize "effectize" o toMarshalcheck

val css = {
    func = Css.summarize,
    print = fn _ => Print.box []
}

val toCss = transform css "css" o toShake5

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

val iflow = {
    func = (fn file => (if !doIflow then Iflow.check file else (); file)),
    print = MonoPrint.p_file MonoEnv.empty
}

val toIflow = transform iflow "iflow" o toMono_opt2

val jscomp = {
    func = JsComp.process,
    print = MonoPrint.p_file MonoEnv.empty
}

val toJscomp = transform jscomp "jscomp" o toIflow

val toMono_opt3 = transform mono_opt "mono_opt3" o toJscomp

val fuse = {
    func = Fuse.fuse,
    print = MonoPrint.p_file MonoEnv.empty
}

val toFuse = transform fuse "fuse" o toMono_opt3

val toUntangle2 = transform untangle "untangle2" o toFuse

val toMono_reduce2 = transform mono_reduce "mono_reduce2" o toUntangle2
val toMono_shake2 = transform mono_shake "mono_shake2" o toMono_reduce2
val toMono_opt4 = transform mono_opt "mono_opt4" o toMono_shake2
val toMono_reduce3 = transform mono_reduce "mono_reduce3" o toMono_opt4
val toFuse2 = transform fuse "fuse2" o toMono_reduce3
val toUntangle3 = transform untangle "untangle3" o toFuse2
val toMono_shake3 = transform mono_shake "mono_shake3" o toUntangle3

val pathcheck = {
    func = (fn file => (PathCheck.check file; file)),
    print = MonoPrint.p_file MonoEnv.empty
}

val toPathcheck = transform pathcheck "pathcheck" o toMono_shake3

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

val checknest = {
    func = fn f => if #supportsNestedPrepared (Settings.currentDbms ()) then f else Checknest.annotate f,
    print = CjrPrint.p_file CjrEnv.empty
}

val toChecknest = transform checknest "checknest" o toPrepare

val sqlify = {
    func = Cjrize.cjrize,
    print = CjrPrint.p_sql CjrEnv.empty
}

val toSqlify = transform sqlify "sqlify" o toMono_opt2

val escapeFilename = String.translate (fn #" " => "\\ " | #"\"" => "\\\"" | #"'" => "\\'" | ch => str ch)

fun compileC {cname, oname, ename, libs, profile, debug, link = link'} =
    let
        val proto = Settings.currentProtocol ()

        val lib = if Settings.getStaticLinking () then
                      #linkStatic proto ^ " " ^ Config.lib ^ "/../liburweb.a"
                  else
                      "-L" ^ Config.lib ^ "/.. -lurweb " ^ #linkDynamic proto

        val compile = Config.ccompiler ^ " " ^ Config.ccArgs ^ " " ^ Config.pthreadCflags ^ " -Wimplicit -Werror -Wno-unused-value -O3 -I " ^ Config.includ
                      ^ " " ^ #compile proto
                      ^ " -c " ^ escapeFilename cname ^ " -o " ^ escapeFilename oname

        val link = Config.ccompiler ^ " -Werror -O3 -lm " ^ Config.ccArgs ^ " " ^ Config.pthreadCflags ^ " " ^ Config.pthreadLibs
                   ^ " " ^ lib ^ " " ^ Config.openssl ^ " " ^ escapeFilename oname ^ " -o " ^ escapeFilename ename ^ " " ^ libs

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

        fun system s =
            (if debug then
                 print (s ^ "\n")
             else
                 ();
             OS.Process.isSuccess (OS.Process.system s))
    in
        system compile andalso system link
    end

fun compile job =
    case run toChecknest job of
        NONE => false
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
                        #link (Settings.currentDbms ())
                    else
                        ""
            in
                Print.fprint s (CjrPrint.p_file CjrEnv.empty file);
		TextIO.output1 (outf, #"\n");
                TextIO.closeOut outf;

                if ErrorMsg.anyErrors () then
                    false
                else
                    (case #sql job of
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
                               profile = #profile job, debug = #debug job, link = #link job}
                     
                     before cleanup ())
            end
            handle ex => (((cleanup ()) handle _ => ()); raise ex)
        end

fun compiler job =
    if compile job then
        ()
    else
        OS.Process.exit OS.Process.failure

fun moduleOf fname =
    let
        val mrs = !moduleRoots
        val fname = OS.Path.mkCanonical fname
    in
        case List.find (fn (root, _) => String.isPrefix (root ^ "/") fname) mrs of
            NONE => capitalize (OS.Path.base (OS.Path.file fname))
          | SOME (root, name) =>
            let
                val fname = OS.Path.base fname
                val fname = String.extract (fname, size root + 1, NONE)
                val fs = String.fields (fn ch => ch = #"/") fname
                val fs = List.filter (CharVector.exists (fn ch => not (Char.isDigit ch))) fs
                val fs = map capitalize fs
            in
                String.concatWith "." (name :: fs)
            end
    end

end
