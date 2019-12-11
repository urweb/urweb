structure C = Compiler
fun debug (str: string): unit =
    (TextIO.output (TextIO.stdErr, str ^ "\n\n"); TextIO.flushOut TextIO.stdErr)

fun trim (s: substring): substring =
    Substring.dropr
        (fn c => c = #" " orelse c = #"\n" orelse c = #"\r")
        (Substring.dropl (fn c => c = #" " orelse c = #"\n" orelse c = #"\r") s)

fun joinStr (sep: string) (strs: string list): string =
    List.foldl (fn (str, acc) => if acc = "" then str else acc ^ sep ^ str) "" strs

structure FromJson = struct 
fun get (s: string) (l: Json.json): Json.json =
      case l of
          Json.Obj pairs =>
          (case List.find (fn tup => #1 tup = s) pairs of
                NONE => raise Fail ("Failed to find JSON object key " ^ s ^ " in " ^ Json.print l)
              | SOME tup => #2 tup)
        | _ => raise Fail ("Expected JSON object, got: " ^ Json.print l)

fun asInt (j: Json.json): int = 
    case j of
        Json.Int i => i
     |  _ => raise Fail ("Expected JSON int, got: " ^ Json.print j)

fun asString (j: Json.json): string =
    case j of
        Json.String s => s
     |  _ => raise Fail ("Expected JSON string, got: " ^ Json.print j)

fun asOptionalInt (j: Json.json): int option =
    case j of
        Json.Null => NONE
     |  Json.Int i => SOME i
     |  _ => raise Fail ("Expected JSON int or null, got: " ^ Json.print j)

fun asOptionalString (j: Json.json): string option =
    case j of
        Json.Null => NONE
     |  Json.String s => SOME s
     |  _ => raise Fail ("Expected JSON string or null, got: " ^ Json.print j)
end

structure LspSpec (* :> LSPSPEC *) = struct 
  fun readHeader (): (string * string) option =
      let 
          val line = TextIO.inputLine TextIO.stdIn
      in 
          case line of
              NONE => OS.Process.exit OS.Process.success
            | SOME str =>
              if Substring.isEmpty (trim (Substring.full str))
              then NONE
              else
                  let 
                      val (key, value) = Substring.splitl (fn c => c <> #":") (Substring.full str)
                  in 
                      if Substring.isEmpty (trim value)
                      then raise Fail ("Failed to parse LSP header: Line is not empty but is also not a valid header: " ^ str)
                      else SOME ( Substring.string (trim key)
                                , Substring.string (trim (Substring.dropl (fn c => c = #":") (trim value))))
                  end
      end

  fun readAllHeaders (): (string * string) list =
      let 
          fun doReadAllHeaders (l: (string * string) list): (string * string) list = 
              case readHeader () of
                  NONE => l
               |  SOME tup => tup :: doReadAllHeaders l

      in 
          doReadAllHeaders []
      end
  datatype message =
           RequestMessage of { id: Json.json, method: string, params: Json.json}
         | Notification of { method: string, params: Json.json}
  fun parseMessage (j: Json.json): message = 
      let 
          val id = SOME (FromJson.get "id" j)
                   handle ex => NONE
          val method = FromJson.asString (FromJson.get "method" j)
          val params = FromJson.get "params" j
      in 
          case id of
              NONE => Notification {method = method, params = params}
            | SOME id => RequestMessage {id = id, method = method, params = params}
      end

  type documentUri = 
       { scheme: string
       , authority: string
       , path: string
       , query: string
       , fragment: string
       }
  fun parseDocumentUri (str: string): documentUri = 
      let
          val str = Substring.full str
          val (scheme, rest) = Substring.splitl (fn c => c <> #":") str
          val (authority, rest) = Substring.splitl (fn c => c <> #"/") (Substring.triml 3 rest (* :// *))
          val (path, rest) = Substring.splitl (fn c => c <> #"?" orelse c <> #"#") rest
          val (query, rest) = if Substring.first rest = SOME #"?"
                              then Substring.splitl (fn c => c <> #"#") (Substring.triml 1 rest (* ? *))
                              else (Substring.full "", rest)
          val fragment = if Substring.first rest = SOME #"#"
                         then (Substring.triml 1 rest (* # *))
                         else Substring.full ""
                                             
      in
          { scheme = Substring.string scheme
          , authority = Substring.string authority
          , path = Substring.string path
          , query = Substring.string query
          , fragment = Substring.string fragment
          }
      end
  fun printDocumentUri (d: documentUri) =
      (#scheme d) ^ "://" ^
      (#authority d) ^
      (#path d) ^ 
      (if #query d <> "" then "?" ^ #query d else "") ^
      (if #fragment d <> "" then "#" ^ #fragment d else "")

  type textDocumentIdentifier = { uri: documentUri}
  fun parseTextDocumentIdentifier (j: Json.json): textDocumentIdentifier = 
      { uri = parseDocumentUri (FromJson.asString (FromJson.get "uri" j))}

  type versionedTextDocumentIdentifier =
       { uri: documentUri
       , version: int option
       }
  fun parseVersionedTextDocumentIdentifier (j: Json.json): versionedTextDocumentIdentifier =
      { uri = parseDocumentUri (FromJson.asString (FromJson.get "uri" j))
      , version = FromJson.asOptionalInt (FromJson.get "version" j)
      }

  type textDocumentItem = {
      uri: documentUri,
      languageId: string,
      version: int, (* The version number of this document (it will increase after each change, including undo/redo). *)
      text: string
  }
  fun parseTextDocumentItem (j: Json.json) =
      { uri = parseDocumentUri (FromJson.asString (FromJson.get "uri" j))
      , languageId = FromJson.asString (FromJson.get "languageId" j)
      , version = FromJson.asInt (FromJson.get "version" j)
      , text = FromJson.asString (FromJson.get "text" j)
      }

  type position = { line: int
                  , character: int 
                  }
  fun parsePosition (j: Json.json) =
      { line = FromJson.asInt (FromJson.get "line" j)
      , character = FromJson.asInt (FromJson.get "character" j)
      }
  fun printPosition (p: position): Json.json = Json.Obj [ ("line", Json.Int (#line p))
                                                        , ("character", Json.Int (#character p))]
                                                  
  type range = { start: position
               , end_: position }
  fun printRange (r: range): Json.json = Json.Obj [ ("start", printPosition (#start r))
                                                  , ("end", printPosition (#end_ r))]

  fun readRequestFromStdIO (): message =
      let 
          val headers = readAllHeaders ()
          val lengthO = List.find (fn (k,v) => k = "Content-Length") headers
          val request = case lengthO of
                            NONE => raise Fail "No header with Content-Length found"
                         |  SOME (k, v) =>
                            case Int.fromString v of
                                NONE => raise Fail ("Couldn't parse content-length from string: " ^ v)
                              | SOME i => TextIO.inputN (TextIO.stdIn, i)
          val parsed = Json.parse request
      in 
          parseMessage parsed
      end

  type hoverReq = { textDocument: textDocumentIdentifier , position: position }
  type hoverResp = {contents: string} option
  fun parseHoverReq (params: Json.json): hoverReq =
      { textDocument = parseTextDocumentIdentifier (FromJson.get "textDocument" params)
      , position = parsePosition (FromJson.get "position" params)
      }
  fun printHoverResponse (resp: hoverResp): Json.json =
      case resp of
          NONE => Json.Null
        | SOME obj => Json.Obj [("contents", Json.String (#contents obj))]

  type didOpenParams = { textDocument: textDocumentItem }
  fun parseDidOpenParams (params: Json.json): didOpenParams =
      { textDocument = parseTextDocumentItem (FromJson.get "textDocument" params) }

  type didChangeParams = { textDocument: versionedTextDocumentIdentifier }
  fun parseDidChangeParams (params: Json.json): didChangeParams =
      { textDocument = parseVersionedTextDocumentIdentifier (FromJson.get "textDocument" params)
      (* , contentChanges = ... *)
      }

  type didSaveParams = { textDocument: textDocumentIdentifier }
  fun parseDidSaveParams (params: Json.json): didSaveParams =
      { textDocument = parseTextDocumentIdentifier (FromJson.get "textDocument" params)
      (* , text = ... *)
      }
  type initializeParams =
       { rootUri: documentUri option }
  fun parseInitializeParams (j: Json.json) =
      { rootUri =
        Option.map
          parseDocumentUri
          (FromJson.asOptionalString (FromJson.get "rootUri" j))
      }
  type diagnostic = { range: range
                    (* code?: number | string *)
                    , severity: int (* 1 = error, 2 = warning, 3 = info, 4 = hint*)
                    , source: string
                    , message: string
                    (* relatedInformation?: DiagnosticRelatedInformation[]; *)
                    }
  fun printDiagnostic (d: diagnostic): Json.json = 
      Json.Obj [ ("range", printRange (#range d))
               , ("severity", Json.Int (#severity d))
               , ("source", Json.String (#source d))
               , ("message", Json.String (#message d))
               ]
  type publishDiagnosticsParams = { uri: documentUri
                                  , diagnostics: diagnostic list
                                  }
  fun printPublishDiagnosticsParams (p: publishDiagnosticsParams): Json.json =
      Json.Obj [ ("uri", Json.String (printDocumentUri (#uri p)))
               , ("diagnostics", Json.Array (List.map printDiagnostic (#diagnostics p)))]
               
  type initializeResponse = { capabilities:
                              { hoverProvider: bool
                              , textDocumentSync:
                                { openClose: bool
                                , save: { includeText: bool } option
                                }
                              }}
  fun printInitializeResponse (res: initializeResponse) = 
      Json.Obj [("capabilities",
                 let
                     val capabilities = #capabilities res
                 in
                     Json.Obj [ ("hoverProvider", Json.Bool (#hoverProvider capabilities))
                              , ("textDocumentSync",
                                 let
                                     val textDocumentSync = #textDocumentSync capabilities
                                 in
                                     Json.Obj [ ("openClose", Json.Bool (#openClose textDocumentSync ))
                                              , ("save", case #save textDocumentSync of
                                                             NONE => Json.Null
                                                           | SOME save => Json.Obj [("includeText", Json.Bool (#includeText save) )])]
                                 end
                              )]
                 end
      )]

  datatype 'a result =
           Success of 'a
         | Error of (int * string)

  fun mapResult (f: 'a -> 'b) (a: 'a result): 'b result =
      case a of
          Success contents => Success (f contents)
        | Error e => Error e
  type toclient = { showMessage: string -> int -> unit
                  , publishDiagnostics: publishDiagnosticsParams -> unit }
  type messageHandlers =
       { initialize: initializeParams -> initializeResponse result
       , shutdown: unit -> unit result
       , textDocument_hover: toclient -> hoverReq -> hoverResp result
       }
      
  fun showMessage str typ =
      let
          val jsonToPrint = Json.print (Json.Obj [ ("jsonrpc", Json.String "2.0")
                                                 , ("method", Json.String "window/showMessage")
                                                 , ("params", Json.Obj [ ("type", Json.Int typ)
                                                                       , ("message", Json.String str)])
                                       ])
          val toPrint = "Content-Length:" ^ Int.toString (String.size jsonToPrint) ^ "\r\n\r\n" ^ jsonToPrint
      in
          TextIO.print toPrint
      end
  fun publishDiagnostics diags =
      let
          val jsonToPrint = Json.print ((Json.Obj [ ("jsonrpc", Json.String "2.0")
                                                  , ("method", Json.String "textDocument/publishDiagnostics")
                                                  , ("params", printPublishDiagnosticsParams diags)
                                       ]))
          val toPrint = "Content-Length:" ^ Int.toString (String.size jsonToPrint) ^ "\r\n\r\n" ^ jsonToPrint
      in
           TextIO.print toPrint
      end
  val toclient: toclient = {showMessage = showMessage, publishDiagnostics = publishDiagnostics}

  fun handleMessage
          (requestMessage: {id: Json.json, method: string, params: Json.json})
          (handlers: messageHandlers)
      : unit =
    let 
        val result: Json.json result = 
            case #method requestMessage of
                "initialize" =>
                mapResult
                    printInitializeResponse
                    ((#initialize handlers)
                       (parseInitializeParams (#params requestMessage)))
              | "textDocument/hover" =>
                mapResult
                    printHoverResponse
                    ((#textDocument_hover handlers)
                         toclient
                         (parseHoverReq (#params requestMessage)))
              | "shutdown" =>
                mapResult
                    (fn () => Json.Null)
                    ((#shutdown handlers) ())
              | "exit" =>
                OS.Process.exit OS.Process.success
              | method => Error (~32601, "Method not supported: " ^ method)
        (* val () = (TextIO.output (TextIO.stdErr, "Got result: " ^ (case result of Success _ => "success\n"  *)
        (*                                                                       |  Error _ => "error\n")); TextIO.flushOut TextIO.stdErr) *)
    in 
        case result of
            Success j =>
            let
                val jsonToPrint =
                    Json.print (Json.Obj [ ("id", #id requestMessage)
                                         , ("jsonrpc", Json.String "2.0")
                                         , ("result", j)
                               ])
                val toPrint = "Content-Length:" ^ Int.toString (String.size jsonToPrint) ^ "\r\n\r\n" ^ jsonToPrint
            in
                TextIO.print toPrint
            end
          | Error (i, err) => 
            let
                val jsonToPrint =
                    Json.print (Json.Obj [ ("id", #id requestMessage)
                                         , ("jsonrpc", Json.String "2.0")
                                         , ("error", Json.Obj [ ("code", Json.Int i)
                                                              , ("message", Json.String err)
                                           ])
                               ])
                val toPrint = "Content-Length:" ^ Int.toString (String.size jsonToPrint) ^ "\r\n\r\n" ^ jsonToPrint
            in
                TextIO.print toPrint
            end
    end

  type notificationHandlers =
       { initialized: unit -> unit
       , textDocument_didOpen: toclient -> didOpenParams -> unit
       , textDocument_didChange: toclient -> didChangeParams -> unit
       , textDocument_didSave: toclient -> didSaveParams -> unit
       }
  fun handleNotification
          (notification: {method: string, params: Json.json})
          (handlers: notificationHandlers)
      =
      case #method notification of
            "initialized" => (#initialized handlers) ()
          | "textDocument/didOpen" => (#textDocument_didOpen handlers) toclient (parseDidOpenParams (#params notification))
          | "textDocument/didChange" => (#textDocument_didChange handlers) toclient (parseDidChangeParams (#params notification))
          | "textDocument/didSave" => (#textDocument_didSave handlers) toclient (parseDidSaveParams (#params notification))
          | m => debug ("Notification method not supported: " ^ m)
      
end

structure Lsp :> LSP = struct 


datatype lspError = InternalError of string
exception LspError of lspError
fun handleLspErrorInNotification (e: lspError) : unit =
    let
        fun print (message: string) =
            let
                val jsonStr = Json.print (Json.Obj [ ("jsonrpc", Json.String "2.0")
                                                   , ("method", Json.String "window/showMessage")
                                                   , ("params", Json.Obj [ ("type", Json.Int 1 (* Error*))
                                                                         , ("message", Json.String message)])
                                                   ])
            in
                TextIO.print ("Content-Length:" ^ Int.toString (String.size jsonStr) ^ "\r\n\r\n" ^ jsonStr)
            end
    in
        case e of
            InternalError str => print str
    end
fun handleLspErrorInRequest (id: Json.json) (e: lspError): unit =
    let
        fun print (code: int) (message: string) =
            let
                val jsonStr = Json.print (Json.Obj [ ("jsonrpc", Json.String "2.0")
                                                   , ("id", id)
                                                   , ("error", Json.Obj [ ("code", Json.Int code (* Error*))
                                                                        , ("message", Json.String message)])
                                                   ])
            in
                TextIO.print ("Content-Length:" ^ Int.toString (String.size jsonStr) ^ "\r\n\r\n" ^ jsonStr)
            end
    in
        case e of
            InternalError str => print (~32603) str
    end

structure SK = struct
  type ord_key = string
  val compare = String.compare
end
structure SM = BinaryMapFn(SK)

type fileState =
     { envOfPreviousModules : ElabEnv.env
     , decls : Elab.decl list
     }
type state =
     { urpPath : string
     , fileStates : fileState SM.map
     }
val stateRef = ref (NONE: state option)

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
        val foundUrps = scanDir (fn fname => OS.Path.ext fname = SOME "urp") rootPath
    in 
        { urpPath = case foundUrps of
                        [] => raise Fail ("No .urp files found in path " ^ rootPath)
                      | one :: [] => OS.Path.base (OS.Path.file one)
                      | many => raise Fail ("Found multiple .urp files in path " ^ rootPath)
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
fun calculateFileState (state: state) (fileName: string): (fileState option * LspSpec.diagnostic list) =
    let
        val () = if (OS.Path.ext fileName = SOME "ur")
                 then ()
                 else raise Fail ("Can only handle .ur files for now")
        val () = Elaborate.unifyMore := true
        (* Parsing .urp *)
        val job = case C.run (C.transform C.parseUrp "parseUrp") (#urpPath state) of
                      NONE => raise LspError (InternalError ("Couldn't parse .urp file at " ^ (#urpPath state)))
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
                (#sources job)
        val modulesBeforeThisFile = #1 moduleSearchRes 
        val () = if #2 moduleSearchRes
                 then ()
                 else raise LspError (InternalError ("Couldn't find file " ^ fileName ^ " referenced in .urp file at " ^ (#urpPath state)))
        (* Parsing .urs files of previous modules *)
        val parsedUrss = List.map (fn entry =>
                                      let
                                          val fileName = entry ^ ".urs"
                                      in
                                          { fileName = fileName
                                          , parsed =
                                            if OS.FileSys.access (fileName, [])
                                            then case C.run (C.transform C.parseUrs "parseUrs") fileName of
                                                     NONE => raise LspError (InternalError ("Failed to parse .urs file at " ^ fileName))
                                                   | SOME a => a
                                            else raise LspError (InternalError ("Couldn't find an .urs file for " ^ fileName))
                                          }
                                      end)
                                  modulesBeforeThisFile
        (* Parsing Basis and Top .urs *)
        val parsedBasisUrs =
            case C.run (C.transform C.parseUrs "parseUrs") (Settings.libFile "basis.urs") of
                NONE => raise LspError (InternalError ("Failed to parse basis.urs file at " ^ (Settings.libFile "basis.urs")))
              | SOME a => a
        val parsedTopUrs =
            case C.run (C.transform C.parseUrs "parseUrs") (Settings.libFile "top.urs") of
                NONE => raise LspError (InternalError ("Failed to parse top.urs file at " ^ (Settings.libFile "top.urs")))
              | SOME a => a
        (* Building env with previous .urs files *)
        val envWithStdLib =
            addSgnToEnv
                (addSgnToEnv ElabEnv.empty parsedBasisUrs (Settings.libFile "basis.urs") true)
                parsedTopUrs (Settings.libFile "top.urs") true
        val envBeforeThisFile = List.foldl (fn (sgn, env) => addSgnToEnv env (#parsed sgn) (#fileName sgn) false) envWithStdLib parsedUrss
        (* Parsing .ur and .urs of current file *)
        val (parsedUrs: (Source.sgn_item list) option) =
            (if OS.FileSys.access (fileName ^ "s", [])
             then
                 case C.run (C.transform C.parseUrs "parseUrs") (fileName ^ "s") of
                     NONE => NONE
                   | SOME a => SOME a
             else
                 NONE) handle ex => NONE
        val () = ErrorMsg.resetErrors ()
        val (parsedUrO: (Source.decl list) option) =
            C.run (C.transform C.parseUr "parseUr") fileName
    in
        case parsedUrO of
            NONE => (* Parse error *) (NONE, List.map errorToDiagnostic (ErrorMsg.readErrorLog ()))
          | SOME parsedUr => 
            (* .ur file found -> typecheck *)
            let 
                val (str, sgn', gs) =
                    Elaborate.elabStr
                        (envBeforeThisFile, Disjoint.empty)
                        (Source.StrConst parsedUr, {file = fileName, first = ErrorMsg.dummyPos, last = ErrorMsg.dummyPos})
                val () =
                    (* .urs file found -> check and compare with .ur file *)
                    (case parsedUrs of
                        NONE => ()
                      | SOME parsedUrs => 
                        let 
                            val (sgn, gs) = Elaborate.elabSgn
                                              (envBeforeThisFile, Disjoint.empty)
                                              ( Source.SgnConst parsedUrs
                                              , {file = fileName ^ "s", first = ErrorMsg.dummyPos, last = ErrorMsg.dummyPos});
                        in
                            Elaborate.subSgn envBeforeThisFile ErrorMsg.dummySpan sgn' sgn
                        end)
                (* report back errors (as Diagnostics) *)
                val errors = ErrorMsg.readErrorLog ()
                val decls = case str of
                                (Elab.StrConst decls, _) => decls
                              | _ => raise Fail ("Impossible: Source.StrConst did not become Elab.StrConst after elaboration")
            in 
                (SOME { envOfPreviousModules = envBeforeThisFile
                      , decls = decls
                      },
                 List.map errorToDiagnostic errors)
            end
    end

fun handleFullDocument (state: state) (toclient: LspSpec.toclient) (documentUri: LspSpec.documentUri) =
    let 
        val path = #path documentUri
        val res = calculateFileState state path
    in
        (case #1 res of 
             NONE => ()
           | SOME fs =>
             stateRef := SOME { urpPath = #urpPath state
                              , fileStates = SM.insert ( #fileStates state
                                                       , path
                                                       , fs)
                              }) ;
        #publishDiagnostics toclient { uri = documentUri , diagnostics = #2 res}
    end

fun serverLoop () =            
    let 
        val requestMessage =
            LspSpec.readRequestFromStdIO ()
            handle ex => (debug (General.exnMessage ex) ; raise ex)
        val state = !stateRef
    in
        case state of
            NONE =>
            (case requestMessage of
                  LspSpec.RequestMessage m =>
                  LspSpec.handleMessage
                    m
                    { initialize = fn _ => 
                                      (let val st = initState (LspSpec.parseInitializeParams (#params m))
                                       in
                                           stateRef := SOME st;
                                           LspSpec.Success
                                               { capabilities =
                                                 { hoverProvider = true
                                                 , textDocumentSync = { openClose = true
                                                                      , save = SOME { includeText = false }
                                                 }}
                                               }
                                       end)
                                      handle (Fail str) => LspSpec.Error (~32602, str)
                    , shutdown = fn () => LspSpec.Error (~32002, "Server not initialized")
                    , textDocument_hover = fn ctx => fn _ => LspSpec.Error (~32002, "Server not initialized")
                    }
                | LspSpec.Notification n => ())
          | SOME state => 
            (case requestMessage of
                LspSpec.Notification n =>
                ((LspSpec.handleNotification
                  n
                  { initialized = fn () => ()
                  , textDocument_didOpen = fn ctx => fn p => handleFullDocument state ctx (#uri (#textDocument p))
                  , textDocument_didChange = fn ctx => fn didChangeParams => ()
                  , textDocument_didSave = fn ctx => fn p => handleFullDocument state ctx (#uri (#textDocument p))
                  })
                 handle LspError e => handleLspErrorInNotification e
                      | ex => handleLspErrorInNotification (InternalError (General.exnMessage ex)))
              | LspSpec.RequestMessage m => 
                (* TODO should error handling here be inside handleMessage? *)
                ((LspSpec.handleMessage
                  m
                  { initialize = fn _ => LspSpec.Error (~32600, "Server already initialized")
                  , shutdown = fn () => LspSpec.Success ()
                  , textDocument_hover = fn ctx => fn _ => LspSpec.Success NONE
                  })
                 handle LspError e => handleLspErrorInRequest (#id m) e
                      | ex => handleLspErrorInRequest (#id m) (InternalError (General.exnMessage ex)))
            ) 
    end

fun startServer () = while true do serverLoop ()
end
