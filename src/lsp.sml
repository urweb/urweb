structure Lsp :> LSP = struct 

structure C = Compiler
structure P = Print

val debug = LspSpec.debug

structure SK = struct
  type ord_key = string
  val compare = String.compare
end
structure SM = BinaryMapFn(SK)

type fileState =
     { envBeforeThisModule: ElabEnv.env
     , decls: Elab.decl list
     , text: string}
type state =
     { urpPath : string
     , fileStates : fileState SM.map
     }

(* Wrapping this in structure as an attempt to not get concurrency bugs *)
structure State :
          sig
              val init: state -> unit
              val insertText: string -> string -> unit
              val insertElabRes: string -> ElabEnv.env -> Elab.decl list -> unit
              val removeFile: string -> unit
              val withState: (state -> 'a) -> 'a
          end = struct
val stateRef = ref (NONE: state option)
fun init (s: state) = 
    stateRef := SOME s
fun withState (f: state -> 'a): 'a =
    case !stateRef of
        NONE => raise LspSpec.LspError LspSpec.ServerNotInitialized
      | SOME s => f s

fun insertText (fileName: string) (text: string) = 
    withState (fn oldS =>
                  stateRef := SOME { urpPath = #urpPath oldS
                                   , fileStates =
                                     case SM.find (#fileStates oldS, fileName) of
                                         NONE => SM.insert ( #fileStates oldS
                                                           , fileName
                                                           , { text = text
                                                             , decls = []
                                                             , envBeforeThisModule = ElabEnv.empty })
                                       | SOME oldfs => 
                                         SM.insert ( #fileStates oldS
                                                   , fileName
                                                   , { text = text
                                                     , decls = #decls oldfs
                                                     , envBeforeThisModule = #envBeforeThisModule oldfs })
                                   }
              )

fun insertElabRes (fileName: string) (env: ElabEnv.env) decls =
    withState (fn oldS =>
                  stateRef := SOME { urpPath = #urpPath oldS
                                   , fileStates =
                                     case SM.find (#fileStates oldS, fileName) of
                                         NONE => raise Fail ("No text found for file " ^ fileName)
                                       | SOME oldfs => 
                                         SM.insert ( #fileStates oldS
                                                   , fileName
                                                   , { text = #text oldfs
                                                     , decls = decls
                                                     , envBeforeThisModule = env })
                                   }
              )

fun removeFile (fileName: string) =
    withState (fn oldS =>
                  stateRef := SOME { urpPath = #urpPath oldS
                                   , fileStates = #1 (SM.remove (#fileStates oldS, fileName))
                                   }
              )

end



fun scanDir (f: string -> bool) (path: string) =
    let
        val dir = OS.FileSys.openDir path
        fun doScanDir acc =
            case OS.FileSys.readDir dir of 
                NONE => (OS.FileSys.closeDir dir; acc)
             |  SOME fname =>
                (if f fname
                 then doScanDir (fname :: acc)
                 else doScanDir acc)
    in
        doScanDir []
    end

(* Throws Fail if can't init *)
fun initState (initParams: LspSpec.initializeParams): state = 
    let 
        val rootPath = case #rootUri initParams of 
                           NONE => raise Fail "No rootdir found"
                         | SOME a => #path a
        val optsUrpFile =
            (SOME (FromJson.asString (FromJson.get "urpfile" (FromJson.get "project" (FromJson.get "urweb" (#initializationOptions initParams))))))
            handle ex => NONE
        val foundUrps = scanDir (fn fname => OS.Path.ext fname = SOME "urp") rootPath
    in 
        { urpPath = case foundUrps of
                        [] => raise Fail ("No .urp files found in path " ^ rootPath)
                      | one :: [] => OS.Path.base (OS.Path.file one)
                      | many => case List.find (fn m => SOME (OS.Path.base (OS.Path.file m)) = optsUrpFile) many of
                                    NONE => raise Fail ("Found multiple .urp files in path " ^ rootPath)
                                  | SOME f => OS.Path.base (OS.Path.file f)
        , fileStates = SM.empty
        }
    end

fun addSgnToEnv (env: ElabEnv.env) (sgn: Source.sgn_item list) (fileName: string) (addUnprefixed: bool): ElabEnv.env =
    let
        val moduleName = C.moduleOf fileName
        val (sgn, gs) = Elaborate.elabSgn (env, Disjoint.empty) (Source.SgnConst sgn, { file = fileName
                                                                                      , first = ErrorMsg.dummyPos
                                                                                      , last = ErrorMsg.dummyPos })
        val () = case gs of
                     [] => ()
                   | _ => (app (fn (_, env, _, c1, c2) =>
                                   Print.prefaces "Unresolved"
                                                  [("c1", ElabPrint.p_con env c1),
                                                   ("c2", ElabPrint.p_con env c2)]) gs;
                           raise Fail ("Unresolved disjointness constraints in " ^ moduleName ^ " at " ^ fileName)) (* TODO Not sure if this is needed for all signatures or only for Basis *)
        val (env', n) = ElabEnv.pushStrNamed env moduleName sgn
        val (_, env') = if addUnprefixed
                        then Elaborate.dopen env' {str = n, strs = [], sgn = sgn}
                        else ([], env')
    in
        env'
    end

fun errorToDiagnostic (err: { span: ErrorMsg.span , message: string }): LspSpec.diagnostic = 
    { range = { start = { line = #line (#first (#span err)) - 1
                        , character = #char (#first (#span err))
                        }
              , end_ = { line = #line (#last (#span err)) - 1
                       , character = #char (#last (#span err))
                       }
              }
    , severity = 1
    , source = "UrWeb"
    , message = #message err
    }

(* TODO FFI modules ? Check compiler.sml -> parse -> parseFfi *)
(* TODO Optim: cache parsed urp file? *)
fun elabFile (state: state) (fileName: string): ({ decls: Elab.decl list, envBeforeThisModule: ElabEnv.env} option * LspSpec.diagnostic list) =
    let
        val () = if (OS.Path.ext fileName = SOME "ur")
                 then ()
                 else raise Fail ("Can only handle .ur files for now")
        (* val () = Elaborate.unifyMore := true *)
        (* To reuse Basis and Top *)
        val () = Elaborate.incremental := true
        (* Parsing .urp *)
        val job = case C.run (C.transform C.parseUrp "parseUrp") (#urpPath state) of
                      NONE => raise LspSpec.LspError (LspSpec.InternalError ("Couldn't parse .urp file at " ^ (#urpPath state)))
                    | SOME a => a
        val moduleSearchRes =
            List.foldl
                (fn (entry, acc) => if #2 acc
                                    then acc
                                    else
                                        if entry ^ ".ur" = fileName
                                        then (List.rev (#1 acc), true)
                                        else (entry :: #1 acc, false))
                ([] (* modules before *), false (* module found *))
                (#ffi job @ #sources job)
        val modulesBeforeThisFile = #1 moduleSearchRes 
        val () = if #2 moduleSearchRes
                 then ()
                 else raise LspSpec.LspError (LspSpec.InternalError ("Couldn't find file " ^ fileName ^ " referenced in .urp file at " ^ (#urpPath state)))
        (* Parsing .urs files of previous modules *)
        val parsedUrss = List.map (fn entry =>
                                      if OS.FileSys.access (entry ^ ".urs", [])
                                      then case C.run (C.transform C.parseUrs "parseUrs") (entry ^ ".urs") of
                                                NONE => raise LspSpec.LspError (LspSpec.InternalError ("Failed to parse .urs file at " ^ entry))
                                              | SOME a => { fileName = entry ^ ".urs", parsed = a}
                                      else
                                          if OS.FileSys.access (entry ^ ".ur", [])
                                          then case C.run (C.transform C.parseUrs "parseUrs") (entry ^ ".ur") of
                                                   NONE => raise LspSpec.LspError (LspSpec.InternalError ("No .urs file found for " ^ entry ^ " and couldn't parse .ur as .urs file"))
                                                 | SOME a => { fileName = entry ^ ".ur" , parsed = a}
                                          else raise LspSpec.LspError (LspSpec.InternalError ("Couldn't find an .ur or .urs file for " ^ entry)))
                                  modulesBeforeThisFile
        (* Parsing Basis and Top *)
        val basisF = Settings.libFile "basis.urs"
        val topF = Settings.libFile "top.urs"
        val topF' = Settings.libFile "top.ur"

        val tm1 = OS.FileSys.modTime topF
        val tm2 = OS.FileSys.modTime topF'

        val parsedBasisUrs =
            case C.run (C.transform C.parseUrs "parseUrs") basisF of
                NONE => raise LspSpec.LspError (LspSpec.InternalError ("Failed to parse basis.urs file at " ^ basisF))
              | SOME a => a
        val parsedTopUrs =
            case C.run (C.transform C.parseUrs "parseUrs") topF of
                NONE => raise LspSpec.LspError (LspSpec.InternalError ("Failed to parse top.urs file at " ^ topF))
              | SOME a => a
        val parsedTopUr =
            case C.run (C.transform C.parseUr "parseUr") topF' of
                NONE => raise LspSpec.LspError (LspSpec.InternalError ("Failed to parse top.ur file at " ^ topF'))
              | SOME a => a

        (* Parsing .ur and .urs of current file *)
        val (parsedUrs: Source.sgn option) =
            (if OS.FileSys.access (fileName ^ "s", [])
             then
                 case C.run (C.transform C.parseUrs "parseUrs") (fileName ^ "s") of
                     NONE => NONE
                   | SOME a => SOME ( Source.SgnConst a
                                    , {file = fileName ^ "s", first = ErrorMsg.dummyPos, last = ErrorMsg.dummyPos})
             else
                 NONE) handle ex => NONE
        val () = ErrorMsg.resetErrors ()
        val (parsedUrO: (Source.decl list) option) =
            C.run (C.transform C.parseUr "parseUr") fileName
    in
        case parsedUrO of
            NONE => (* Parse error *) (NONE, List.map errorToDiagnostic (ErrorMsg.readErrorLog ()))
          | SOME parsedUr => 
            (* Parsing of .ur succeeded *)
            let 
                val loc = {file = fileName, first = ErrorMsg.dummyPos, last = ErrorMsg.dummyPos}
                val envBeforeThisModule = ref ElabEnv.empty
                val res = Elaborate.elabFile
                              parsedBasisUrs tm1 parsedTopUr parsedTopUrs tm2 ElabEnv.empty
                              (* Adding urs's of previous modules to env *)
                              (fn envB =>
                                  let 
                                      val newEnv = List.foldl (fn (sgn, env) => addSgnToEnv env (#parsed sgn) (#fileName sgn) false) envB parsedUrss
                                  in
                                      (envBeforeThisModule := newEnv; newEnv)
                                  end
                              )
                              [( Source.DStr (C.moduleOf fileName, parsedUrs, NONE, (Source.StrConst parsedUr, loc), false)
                               , loc )]
                (* report back errors (as Diagnostics) *)
                val errors = ErrorMsg.readErrorLog ()
                val decls = case List.last res of
                                (Elab.DStr (_, _, _, (Elab.StrConst decls, _)), _) => decls
                              | _ => raise Fail ("Impossible: Source.StrConst did not become Elab.StrConst after elaboration")
            in 
                (SOME { envBeforeThisModule = !envBeforeThisModule, decls = decls },
                 List.map errorToDiagnostic errors)
            end
    end

fun uniq (eq: 'b -> 'b -> bool) (bs: 'b list) =
    case bs of
        [] => []
      | (l as b :: bs') => b :: uniq eq (List.filter (fn a => not (eq a b)) bs')

fun elabFileAndSendDiags (state: state) (toclient: LspSpec.toclient) (documentUri: LspSpec.documentUri): unit =
    let 
        val fileName = #path documentUri
        val res = elabFile state fileName
        fun eq_diag (d1: LspSpec.diagnostic) (d2: LspSpec.diagnostic) = #range d1 = #range d2 andalso #message d1 = #message d2
        val diags = uniq eq_diag (#2 res)
    in
        (case #1 res of
             NONE => ()
           | SOME fs =>
             (State.insertElabRes fileName (#envBeforeThisModule fs) (#decls fs));
         #publishDiagnostics toclient { uri = documentUri , diagnostics = diags})
    end

fun scanDir (f: string -> bool) (path: string) =
    let
        val dir = OS.FileSys.openDir path
        fun doScanDir acc =
            case OS.FileSys.readDir dir of 
                NONE => (OS.FileSys.closeDir dir; acc)
             |  SOME fname =>
                (if f fname
                 then doScanDir (fname :: acc)
                 else doScanDir acc)
    in
        doScanDir []
    end

fun readFile (fileName: string): string =
    let 
        val stream = TextIO.openIn fileName
        fun doReadFile acc = 
            case TextIO.inputLine stream of
                NONE => acc
              | SOME str => (if acc = ""
                             then doReadFile str
                             else doReadFile (acc ^ str))
        val res = doReadFile ""
    in 
        (TextIO.closeIn stream; res)
    end
        

(* TODO PERF BIG I couldn't figure out how to print just to a string, so writing to a temp file, then reading it, then deleting it, ... *)
fun ppToString (pp: Print.PD.pp_desc) (width: int): string =
    let
        val tempfile = OS.FileSys.tmpName ()
        val outStr = TextIO.openOut tempfile
        val outDev = TextIOPP.openOut {dst = outStr, wid = width}
        val () = Print.fprint outDev pp
        val res = readFile tempfile
        val () = TextIO.closeOut outStr
    in
        res
    end

fun getStringAtCursor
    (stopAtCursor: bool)
    (text: string)
    (pos: LspSpec.position)
    : string
    = 
    let
        val line = List.nth (Substring.fields (fn c => c = #"\n") (Substring.full text), #line pos)
        val chars = [ (* #".", *) #"(", #")", #"{", #"}", #"[", #"]", #"<", #">", #"-", #"=", #":", #"@"
                      , #" ", #"\n", #"#", #",", #"*", #"\"", #"|", #"&", #"$", #"^", #"+", #";"]
        val lineUntilCursor = Substring.slice (line, 0, SOME (#character pos))
        val beforeCursor = Substring.string (Substring.taker (fn c => not (List.exists (fn c' => c = c') chars)) lineUntilCursor)
        val afterCursor = if stopAtCursor
                          then ""
                          else let
                              val lineAfterCursor = Substring.slice (line, #character pos, NONE)
                          in
                              Substring.string (Substring.takel (fn c => not (List.exists (fn c' => c = c') (#"." :: chars))) lineAfterCursor)
                          end
    in
        beforeCursor ^ afterCursor
    end

fun formatTypeBox (a: P.PD.pp_desc, b: P.PD.pp_desc) =
    P.PD.hvBox (P.PD.PPS.Rel 0, [a,
                                 P.PD.string ": ",
                                 P.PD.break {nsp = 0, offset = 2},
                                 b])

fun handleHover (state: state) (p: LspSpec.hoverReq): LspSpec.hoverResp LspSpec.result =
    let
        val fileName = #path (#uri (#textDocument p))
        val s = SM.find (#fileStates state, fileName)
    in
        case s of
            NONE => LspSpec.Success NONE
          | SOME s =>
            let 
                val searchString = getStringAtCursor false (#text s) (#position p)
                val env = #envBeforeThisModule s
                val decls = #decls s
                val loc = #position p
                val (env, prefix, found) = GetInfo.findStringInEnv env (Elab.StrConst decls) fileName { line = #line loc + 1
                                                                                                      , char = #character loc + 1} searchString
            in
                case found of
                    NONE => LspSpec.Success NONE
                  | SOME f =>
                    let
                        val desc = case f of
                                       GetInfo.FoundStr (x, (_, sgn)) => formatTypeBox (P.PD.string (prefix ^ x), P.PD.string "module")
                                     | GetInfo.FoundKind (x, kind) => formatTypeBox (P.PD.string (prefix ^ x), ElabPrint.p_kind env kind)
                                     | GetInfo.FoundCon (x, con) => formatTypeBox (P.PD.string (prefix ^ x), ElabPrint.p_con env con)
                    in
                        LspSpec.Success (SOME {contents = ppToString desc 50})
                    end
            end
    end

(* TODO IDEA can we use the real parser to figure out what we're typing (exp, con, field, etc) to predict better? *)
fun handleCompletion (state: state) (p: LspSpec.completionReq) =
    let
        val fileName = #path (#uri (#textDocument p))
        val s = SM.find (#fileStates state, fileName)
    in
        case s of
            NONE => LspSpec.Success { isIncomplete = false, items = []}
          | SOME s =>
            let 
                val pos = #position p
                val searchStr = getStringAtCursor true (#text s) pos
                val env = #envBeforeThisModule s
                val decls = #decls s
                val (env, prefix, foundItems) = GetInfo.matchStringInEnv env (Elab.StrConst decls) fileName { line = #line pos + 1, char = #character pos + 1} searchStr
                val completions = List.map
                                    (fn f => case f of
                                                GetInfo.FoundStr (x, _) => {label = prefix ^ x, kind = LspSpec.Module, detail = ""}
                                              | GetInfo.FoundKind (x, k) => {label = prefix ^ x, kind = LspSpec.Constructor, detail = ppToString (ElabPrint.p_kind env k) 200}
                                              | GetInfo.FoundCon (x, c) => {label = prefix ^ x, kind = LspSpec.Value, detail = ppToString (ElabPrint.p_con env c) 200}
                                    )
                                    foundItems
            in
                LspSpec.Success { isIncomplete = false
                                , items = completions }
            end
    end

fun applyContentChange ((c, s): LspSpec.contentChange * string): string =
    case (#range c, #rangeLength c) of
        (SOME range, SOME _) =>
        let
            val lines = Substring.fields (fn c => c = #"\n") (Substring.full s)
            val linesBefore = List.take (lines, #line (#start range))
            val linesAfter = List.drop (lines, #line (#end_ range) + 1)
            val startLine = List.nth (lines, #line (#start range))
            val startText = Substring.slice (startLine, 0, SOME (#character (#start range))) 
            val endLine = List.nth (lines, #line (#end_ range))
            val endText = Substring.triml (#character (#end_ range)) endLine 
        in
            Substring.concatWith "\n" (linesBefore
                                       @ [Substring.full (Substring.concat [startText, Substring.full (#text c), endText])]
                                       @ linesAfter)
        end
      | _ =>
        #text c

fun handleDocumentDidChange (state: state) (toclient: LspSpec.toclient) (p: LspSpec.didChangeParams): unit =
    let
        val fileName = #path (#uri (#textDocument p))
        val s = SM.find (#fileStates state, fileName)
    in
        case s of
            NONE =>
            (debug ("Got change event for file that isn't open: " ^ fileName);
             (#showMessage toclient) ("Got change event for file that isn't open: " ^ fileName) 1)
          | SOME s =>
            State.insertText fileName (List.foldl applyContentChange (#text s) (#contentChanges p))
    end

fun runInBackground (toclient: LspSpec.toclient) (fileName: string) (f: unit -> unit): unit =
    BgThread.queueBgTask
        fileName
        ((fn () => (f ()
                    handle LspSpec.LspError (LspSpec.InternalError str) => (#showMessage toclient) str 1
                         | LspSpec.LspError LspSpec.ServerNotInitialized => (#showMessage toclient) "Server not initialized" 1
                         | ex => (#showMessage toclient) (General.exnMessage ex) 1
                   ; (#showMessage toclient) ("Done running BG job for " ^ fileName) 3
                   )))

fun handleRequest (requestMessage: LspSpec.message) =
    case requestMessage of
        LspSpec.Notification n =>
        LspSpec.matchNotification
            n
            { initialized = fn () => ()
            , textDocument_didOpen =
              fn (p, toclient) =>
                 (State.insertText (#path (#uri (#textDocument p))) (#text (#textDocument p));
                  runInBackground
                      toclient
                      (#path (#uri (#textDocument p)))
                      (fn () => State.withState (fn state => elabFileAndSendDiags state toclient (#uri (#textDocument p)))))
            , textDocument_didChange =
              fn (p, toclient) =>
                 State.withState (fn state => handleDocumentDidChange state toclient p)
            , textDocument_didSave =
              fn (p, toclient) =>
                 runInBackground
                     toclient
                     (#path (#uri (#textDocument p)))
                     (fn () => State.withState (fn state => elabFileAndSendDiags state toclient (#uri (#textDocument p))))
            , textDocument_didClose =
              fn (p, toclient) =>
                 State.removeFile (#path (#uri (#textDocument p)))
            }
      | LspSpec.RequestMessage m => 
        (* TODO should error handling here be inside handleMessage? *)
        LspSpec.matchMessage
            m
            { initialize = fn p => 
                              (let val st = initState p
                               in
                                   State.init st;
                                   LspSpec.Success
                                       { capabilities =
                                         { hoverProvider = true
                                         , completionProvider = SOME { triggerCharacters = ["."]}
                                         , textDocumentSync = { openClose = true
                                                              , change = 2
                                                              , save = SOME { includeText = false }
                                         }}
                                       }
                               end)
            , shutdown = fn () => LspSpec.Success ()
            , textDocument_hover = fn toclient => State.withState handleHover
            , textDocument_completion = fn p => State.withState (fn s => handleCompletion s p)
        }

fun serverLoop () =            
    if not (Option.isSome (TextIO.canInput (TextIO.stdIn, 1))) andalso BgThread.hasBgTasks ()
    then 
        (* no input waiting -> give control to lower prio thread *)
        BgThread.runBgTaskForABit ()
    else
        let 
            val requestMessage =
                LspSpec.readRequestFromStdIO ()
                handle ex => (debug ("Error in reading from stdIn: " ^ General.exnMessage ex) ; raise ex)
        in
            handleRequest requestMessage
        end

fun startServer () = while true do serverLoop ()
end
