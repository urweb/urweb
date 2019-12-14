structure C = Compiler

structure Lsp :> LSP = struct 

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
                                      let
                                          val fileName = entry ^ ".urs"
                                      in
                                          { fileName = fileName
                                          , parsed =
                                            if OS.FileSys.access (fileName, [])
                                            then case C.run (C.transform C.parseUrs "parseUrs") fileName of
                                                     NONE => raise LspSpec.LspError (LspSpec.InternalError ("Failed to parse .urs file at " ^ fileName))
                                                   | SOME a => a
                                            else raise LspSpec.LspError (LspSpec.InternalError ("Couldn't find an .urs file for " ^ fileName))
                                          }
                                      end)
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

fun elabFileAndSendDiags (state: state) (toclient: LspSpec.toclient) (documentUri: LspSpec.documentUri): unit =
    let 
        val fileName = #path documentUri
        val res = elabFile state fileName
    in
        (case #1 res of
             NONE => ()
           | SOME fs =>
             (State.insertElabRes fileName (#envBeforeThisModule fs) (#decls fs));
         #publishDiagnostics toclient { uri = documentUri , diagnostics = #2 res})
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

fun handleHover (state: state) (p: LspSpec.hoverReq): LspSpec.hoverResp LspSpec.result =
    let
        val fileName = #path (#uri (#textDocument p))
        val s = SM.find (#fileStates state, fileName)
    in
        case s of
            NONE => LspSpec.Success NONE
          | SOME s =>
            let 
                val env = #envBeforeThisModule s
                val decls = #decls s
                val loc = #position p
                val result = GetInfo.getInfo env (Elab.StrConst decls) fileName { line = #line loc + 1
                                                                                , character = #character loc + 1} 
            in
                case #smallestgoodpart result of
                    NONE => LspSpec.Success NONE
                  | SOME {desc = desc, ...} =>
                    LspSpec.Success (SOME {contents = ppToString desc 50})
            end
    end

fun getCompletionsFromFields (env: ElabEnv.env) (prefix: string) (searchStr: string) (fields: (Elab.con * Elab.con) list): LspSpec.completionItem list =
    let
        fun mapF (c1, c2) =
            case c1 of
                (Elab.CName fieldName, _) =>
                if String.isPrefix searchStr fieldName
                then SOME { label = prefix ^ fieldName
                          , kind = LspSpec.Field
                          , detail = ppToString (ElabPrint.p_con env c2) 200
                          }
                else NONE
              | _ => NONE
    in
        List.mapPartial mapF fields
    end

fun getCompletionsFromSignatureItems (env: ElabEnv.env) (prefix: string) (searchStr: string) (items: Elab.sgn_item list): LspSpec.completionItem list =
    let
        fun mapF item =
            case item of
                (Elab.SgiVal (name, _, con), _) =>
                if String.isPrefix searchStr name
                then [{ label = prefix ^ name
                      , kind = LspSpec.Value
                      , detail = ppToString (ElabPrint.p_con env con) 200
                      }]
                else []
              | (Elab.SgiCon (name, _, _, con), _) =>
                if String.isPrefix searchStr name
                then [{ label = prefix ^ name
                      , kind = LspSpec.Value
                      , detail = ppToString (ElabPrint.p_con env con) 200
                      }]
                else []
              | (Elab.SgiDatatype cs, _) =>
                (List.concat
                  (List.map (fn (constr as (dtName, n, xs, constrs)) =>
                                (* Copied from elab_print *)
                                let
                                    val k = (Elab.KType, ErrorMsg.dummySpan)
                                    val env = ElabEnv.pushCNamedAs env dtName n k NONE
                                    val env = List.foldl (fn (x, env) => ElabEnv.pushCRel env x k) env xs
                                    val typeVarsString = List.foldl (fn (x, acc) => acc ^ " " ^ x) "" xs
                                in
                                    List.mapPartial (fn (constrName, _, conO) =>
                                                        if String.isPrefix searchStr constrName
                                                        then SOME { label = prefix ^ constrName
                                                                  , kind = LspSpec.Function
                                                                  , detail = case conO of
                                                                                 NONE => dtName ^ typeVarsString
                                                                               | SOME con => ppToString (ElabPrint.p_con env con) 200 ^ " -> " ^ dtName ^ typeVarsString
                                                                  }
                                                        else NONE) constrs
                                end)
                            cs))
              | (Elab.SgiDatatypeImp _, _) =>
                (* TODO ??? no idea what this is *)
                []
              | (Elab.SgiStr (_, name,  _, _), _) =>
                if String.isPrefix searchStr name
                then [{ label = prefix ^ name
                      , kind = LspSpec.Module
                      , detail = ""
                      }]
                else []
              | (Elab.SgiClass (name, _,  _, con), _) =>
                if String.isPrefix searchStr name
                then [{ label = prefix ^ name
                      , kind = LspSpec.Class
                      , detail = ppToString (ElabPrint.p_con env con) 200
                      }]
                else []
              | _ => []
    in
        List.concat (List.map mapF items)
    end

(* TODO TOCHECK look at con's to specify "kind" more accurately *)
fun findMatchingStringInEnv (env: ElabEnv.env) (str: string): LspSpec.completionItem list =
    let
        val splitted = Substring.fields (fn c => c = #".") (Substring.full str)
    in
        case splitted of
            (_ :: []) =>
            if str = ""
            then []
            else
                let
                    val matchingEs = ElabEnv.matchEByPrefix env str (* function params, let bindings and top-level bindings. Should we discern between Rel and Named? *)
                    val expressionCompletions = List.map (fn (name,con) =>
                                                             { label = name
                                                             , kind = LspSpec.Value
                                                             , detail = ppToString (ElabPrint.p_con env con) 200
                                                             }) matchingEs
                    val matchingStrs = ElabEnv.matchStrByPrefix env str
                    val structureCompletions = List.map (fn (name,(_,sgn)) =>
                                                            { label = name
                                                            , kind = LspSpec.Module
                                                            , detail = ""
                                                            }) matchingStrs
                    val matchingCons = ElabEnv.matchCByPrefix env str
                    val conCompletions = List.map (fn (name,kind) =>
                                                      { label = name
                                                      , kind = LspSpec.Constructor (* TODO probably wrong... *)
                                                      , detail = ppToString (ElabPrint.p_kind env kind) 200
                                                      }) matchingCons
                in
                    expressionCompletions @ structureCompletions @ conCompletions
                end
          | (r :: str :: []) =>
            if Char.isUpper (Substring.sub (r, 0))
            then
                (* Completing STRUCTURE *)
                let 
                    (* TODO PERF SMALL: first match and then equal is not perfect *)
                    val foundStrs = ElabEnv.matchStrByPrefix env (Substring.string r)
                    val filteredStrs = List.filter (fn (name,_) => name = Substring.string r) foundStrs
                in
                    (case List.map (fn (name, (i, sgn)) => (name, ElabEnv.hnormSgn env sgn)) filteredStrs of
                         [] => []
                       | (name, (Elab.SgnConst sgis, _)) :: _ =>
                         getCompletionsFromSignatureItems env (name ^ ".") (Substring.string str) sgis
                       | _ => [])
                end
            else
                (* Completing RECORD *)
                (* TODO TOCHECK is it correct to first try RelE and then NamedE? *)
                let 
                    (* TODO PERF SMALL: first match and then equal is not perfect *)
                    val foundEs = ElabEnv.matchEByPrefix env (Substring.string r)
                    val filteredEs = List.filter (fn (name,_) => name = Substring.string r) foundEs
                in
                    (case List.map (fn (name, c) => (name, ElabOps.reduceCon env c)) filteredEs of
                         [] => []
                       | (name, (Elab.TRecord (Elab.CRecord (_, fields), l2_), l1_)) :: _ =>
                         getCompletionsFromFields env (name ^ ".") (Substring.string str) fields
                       | (name, (* TODO this doesn't always work. I've only managed to get it working for tables in a different module *)
                          ( ( Elab.CApp
                              ( ( (Elab.CApp
                                  ( ( Elab.CModProj (_, _, "sql_table")
                                      , l4_)
                                  , ( Elab.CRecord (_, fields)
                                    , l3_)))
                                  , l2_)
                                , _))
                            , l1_)) :: _ =>
                         (debug "!!"; getCompletionsFromFields env (name ^ ".") (Substring.string str) fields)
                       | _ => [])
                end
          | _ =>
            (* TODO NOTIMPLEMENTED submodules / nested records *)
            []
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
                val line = List.nth (Substring.fields (fn c => c = #"\n") (Substring.full (#text s)), #line pos)
                val () = debug ("line" ^ Substring.string line)
                val chars = [ (* #".", *) #"(", #")", #"{", #"}", #"[", #"]", #"<", #">", #"-", #"=", #":"
                              , #" ", #"\n", #"#", #",", #"*", #"\"", #"|", #"&", #"$", #"^", #"+", #";"]
                val lineUntilPos = Substring.slice (line, 0, SOME (#character pos))
                val () = debug ("lineUntilPos: \"" ^ Substring.string lineUntilPos ^ "\"")
                val searchStr = Substring.string (Substring.taker (fn c => not (List.exists (fn c' => c = c') chars)) lineUntilPos)
                val () = debug ("Looking for completions for: \"" ^ searchStr ^ "\"")
                val env = #envBeforeThisModule s
                val decls = #decls s
                val getInfoRes = GetInfo.getInfo env (Elab.StrConst decls) fileName { line = #line pos + 1
                                                                                    , character = #character pos + 1} 
                val envOfSmallest = #env (#smallest getInfoRes)
            in
                LspSpec.Success { isIncomplete = false
                                , items = findMatchingStringInEnv envOfSmallest searchStr}
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

fun handleRequest (requestMessage: LspSpec.message) =
    case requestMessage of
        LspSpec.Notification n =>
        (LspSpec.matchNotification
             n
             { initialized = fn () => ()
             , textDocument_didOpen =
               fn (p, toclient) => State.withState (fn state =>
                                                       (State.insertText (#path (#uri (#textDocument p))) (#text (#textDocument p)) ;
                                                        elabFileAndSendDiags state toclient (#uri (#textDocument p))))
             , textDocument_didChange =
               fn (p, toclient) => State.withState (fn state => handleDocumentDidChange state toclient p)
             , textDocument_didSave =
               fn (p, toclient) => State.withState (fn state => elabFileAndSendDiags state toclient (#uri (#textDocument p)))
             , textDocument_didClose =
               fn (p, toclient) => State.removeFile (#path (#uri (#textDocument p)))
        })
      | LspSpec.RequestMessage m => 
        (* TODO should error handling here be inside handleMessage? *)
        (LspSpec.matchMessage
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
             , textDocument_completion = State.withState handleCompletion
        })

fun serverLoop () =            
    let 
        val requestMessage =
            LspSpec.readRequestFromStdIO ()
            handle ex => (debug (General.exnMessage ex) ; raise ex)
    in
        handleRequest requestMessage
    end

fun startServer () = while true do serverLoop ()
end
