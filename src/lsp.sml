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
fun getO (s: string) (l: Json.json): Json.json option = 
    case l of
        Json.Obj pairs =>
        (case List.find (fn tup => #1 tup = s) pairs of
             NONE => NONE
           | SOME tup => SOME (#2 tup))
      | _ => raise Fail ("Expected JSON object, got: " ^ Json.print l)
fun get (s: string) (l: Json.json): Json.json =
        (case getO s l of
             NONE => raise Fail ("Failed to find JSON object key " ^ s ^ " in " ^ Json.print l)
           | SOME a => a)

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
  fun parseRange (j: Json.json): range =
      { start = parsePosition (FromJson.get "start" j)
      , end_ = parsePosition (FromJson.get "end" j)
      }
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

  type contentChange = { range: range option
                       , rangeLength: int option
                       , text: string }
  type didChangeParams =
       { textDocument: versionedTextDocumentIdentifier
       , contentChanges: contentChange list
       }
  fun parseDidChangeParams (params: Json.json): didChangeParams =
      { textDocument = parseVersionedTextDocumentIdentifier (FromJson.get "textDocument" params)
      , contentChanges = case FromJson.get "contentChanges" params of
                             Json.Array js =>
                             List.map (fn j => { range = Option.map parseRange (FromJson.getO "range" j)
                                               , rangeLength = Option.map FromJson.asInt (FromJson.getO "rangeLength" j)
                                               , text = FromJson.asString (FromJson.get "text" j)
                                               }
                             ) js
                          | j => raise Fail ("Expected JSON array, got: " ^ Json.print j) 
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

  type completionReq =
       { textDocument: textDocumentIdentifier
       , position: position
       , context: { triggerCharacter: string option
                  , triggerKind: int (* 1 = Invoked = typing an identifier or manual invocation or API
                                        2 = TriggerCharacter
                                        3 = TriggerForIncompleteCompletions*)} option
       } 
  fun parseCompletionReq (j: Json.json): completionReq = 
      { textDocument = parseTextDocumentIdentifier (FromJson.get "textDocument" j)
      , position = parsePosition (FromJson.get "position" j)
      , context = case FromJson.getO "context" j of
                      NONE => NONE
                    | SOME ctx => SOME { triggerCharacter = Option.map FromJson.asString (FromJson.getO "triggerCharacter" ctx)
                                       , triggerKind = FromJson.asInt (FromJson.get "triggerKind" ctx)
                                       }
      }

  datatype completionItemKind = Text | Method | Function | Constructor | Field | Variable | Class | Interface | Module | Property | Unit | Value | Enum | Keyword | Snippet | Color | File | Reference | Folder | EnumMember | Constant | Struct | Event | Operator | TypeParameter
  fun completionItemKindToInt (a: completionItemKind) =
       case a of
	         Text => 1
	       | Method => 2
	       | Function => 3
	       | Constructor => 4
	       | Field => 5
	       | Variable => 6
	       | Class => 7
	       | Interface => 8
	       | Module => 9
	       | Property => 10
	       | Unit => 11
	       | Value => 12
	       | Enum => 13
	       | Keyword => 14
	       | Snippet => 15
	       | Color => 16
	       | File => 17
	       | Reference => 18
	       | Folder => 19
	       | EnumMember => 20
	       | Constant => 21
	       | Struct => 22
	       | Event => 23
	       | Operator => 24
	       | TypeParameter => 25
           
  type completionItem = { label: string
                        , kind: completionItemKind
                        , detail: string
                        }
  type completionResp = { isIncomplete: bool
                        , items: completionItem list
                        }

  fun printCompletionItem (a: completionItem): Json.json = 
      Json.Obj [ ("label", Json.String (#label a))
               , ("kind", Json.Int (completionItemKindToInt (#kind a)))
               , ("detail", Json.String (#detail a))
               ]
  fun printCompletionResp (a: completionResp): Json.json = 
      Json.Obj [ ("isIncomplete", Json.Bool (#isIncomplete a))
               , (("items", Json.Array (List.map printCompletionItem (#items a))))]

  type initializeResponse = { capabilities:
                              { hoverProvider: bool
                              , completionProvider: {triggerCharacters: string list} option
                              , textDocumentSync:
                                { openClose: bool
                                , change: int (* 0 = None, 1 = Full, 2 = Incremental *) 
                                , save: { includeText: bool } option
                                }
                              }}
  fun printInitializeResponse (res: initializeResponse) = 
      Json.Obj [("capabilities",
                 let
                     val capabilities = #capabilities res
                 in
                     Json.Obj [ ("hoverProvider", Json.Bool (#hoverProvider capabilities))
                              , ("completionProvider", case #completionProvider capabilities of
                                                           NONE => Json.Null
                                                         | SOME cp => Json.Obj [("triggerCharacters", Json.Array (List.map Json.String (#triggerCharacters cp)))]
                                )
                              , ("textDocumentSync",
                                 let
                                     val textDocumentSync = #textDocumentSync capabilities
                                 in
                                     Json.Obj [ ("openClose", Json.Bool (#openClose textDocumentSync ))
                                              , ("change", Json.Int (#change textDocumentSync))
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
       , textDocument_completion: completionReq -> completionResp result
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
              | "textDocument/completion" =>
                mapResult
                    printCompletionResp
                    ((#textDocument_completion handlers)
                         (parseCompletionReq (#params requestMessage)))
              | "shutdown" =>
                mapResult
                    (fn () => Json.Null)
                    ((#shutdown handlers) ())
              | "exit" =>
                OS.Process.exit OS.Process.success
              | method => (debug ("Method not supported: " ^ method);
                           Error (~32601, "Method not supported: " ^ method))
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
     { envBeforeThisModule: ElabEnv.env
     , decls: Elab.decl list
     , text: string}
type state =
     { urpPath : string
     , fileStates : fileState SM.map
     }
val stateRef = ref (NONE: state option)

fun insertFileState (state: state) (fileName: string) (fs: fileState) =
    stateRef := SOME { urpPath = #urpPath state
                     , fileStates = SM.insert ( #fileStates state
                                              , fileName
                                              , fs)
                     }

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
        (* Parsing Basis and Top *)
        val basisF = Settings.libFile "basis.urs"
        val topF = Settings.libFile "top.urs"
        val topF' = Settings.libFile "top.ur"

        val tm1 = OS.FileSys.modTime topF
        val tm2 = OS.FileSys.modTime topF'

        val parsedBasisUrs =
            case C.run (C.transform C.parseUrs "parseUrs") basisF of
                NONE => raise LspError (InternalError ("Failed to parse basis.urs file at " ^ basisF))
              | SOME a => a
        val parsedTopUrs =
            case C.run (C.transform C.parseUrs "parseUrs") topF of
                NONE => raise LspError (InternalError ("Failed to parse top.urs file at " ^ topF))
              | SOME a => a
        val parsedTopUr =
            case C.run (C.transform C.parseUr "parseUr") topF' of
                NONE => raise LspError (InternalError ("Failed to parse top.ur file at " ^ topF'))
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

fun handleDocumentSavedOrOpened (state: state) (toclient: LspSpec.toclient) (documentUri: LspSpec.documentUri) (textO: string option) =
    let 
        val fileName = #path documentUri
        val res = elabFile state fileName
        val text = case textO of
                       NONE => (case SM.find (#fileStates state, fileName) of
                                    NONE => ((#showMessage toclient) ("No previous state for file " ^ fileName) 2; NONE)
                                  | SOME previousState => SOME (#text previousState))
                     | SOME text => SOME text
    in
        case text of
            NONE => ()
          | SOME text => 
            (case #1 res of
                 NONE =>
                 insertFileState state fileName { text = text
                                                , envBeforeThisModule = ElabEnv.empty
                                                , decls = [] }
               | SOME fs =>
                 (insertFileState state fileName { text = text
                                                 , envBeforeThisModule = #envBeforeThisModule fs
                                                 , decls = #decls fs });
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
        val str = TextIO.openIn fileName
        fun doReadFile acc = 
            case TextIO.inputLine str of
                NONE => acc
              | SOME str => (str ^ "\n" ^ acc)
        val res = doReadFile ""
    in 
        (TextIO.closeIn str; res)
    end
        

(* TODO I couldn't figure out how to print just to a string, so writing to a temp file, then reading it, then deleting it, ... *)
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
                    LspSpec.Success (SOME {contents = ppToString desc 70})
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
                          , detail = ppToString (ElabPrint.p_con env c2) 150
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
                      , detail = ppToString (ElabPrint.p_con env con) 150
                      }]
                else []
              | (Elab.SgiCon (name, _, _, con), _) =>
                if String.isPrefix searchStr name
                then [{ label = prefix ^ name
                      , kind = LspSpec.Variable
                      , detail = ppToString (ElabPrint.p_con env con) 150
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
                                in
                                    List.mapPartial (fn (constrName, _, conO) =>
                                                        if String.isPrefix searchStr constrName
                                                        then SOME { label = prefix ^ constrName
                                                                  , kind = LspSpec.Function
                                                                  , detail = case conO of
                                                                                 NONE => dtName
                                                                               | SOME con => ppToString (ElabPrint.p_con env con) 150 ^ " -> " ^ dtName
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
                      , detail = ppToString (ElabPrint.p_con env con) 150
                      }]
                else []
              | _ => []
    in
        List.concat (List.map mapF items)
    end

fun findMatchingStringInEnv (env: ElabEnv.env) (str: string): LspSpec.completionItem list =
    let
        val splitted = Substring.fields (fn c => c = #".") (Substring.full str)
    in
        case splitted of
            (_ :: []) =>
            if str = ""
            then []
            else (List.map (fn (name,con) =>
                               { label = name
                               , kind = LspSpec.Variable
                               , detail = ppToString (ElabPrint.p_con env con) 150
                               })
              (ElabEnv.matchRelEByPrefix env str
               @ ElabEnv.matchNamedEByPrefix env str))
          | (r :: str :: []) =>
            if Char.isUpper (Substring.sub (r, 0))
            then  (* r should be a structure *)
                let 
                    (* TODO Perf: first match and then equal is not perfect *)
                    val foundStrs = ElabEnv.matchStrByPrefix env (Substring.string r)
                    val filteredStrs = List.filter (fn (name,_) => name = Substring.string r) foundStrs
                in
                    (case List.map (fn (name, (i, sgn)) => (name, ElabEnv.hnormSgn env sgn)) filteredStrs of
                         [] => []
                       | (name, (Elab.SgnConst sgis, _)) :: _ =>
                         getCompletionsFromSignatureItems env (name ^ ".") (Substring.string str) sgis
                       | _ => [])
                end
            else (* r should be a record *)
                (* TODO is it correct to first try RelE and then NamedE? *)
                let 
                    (* TODO Perf: first match and then equal is not perfect *)
                    val foundRelEs = ElabEnv.matchRelEByPrefix env (Substring.string r)
                    val foundNamedEs = ElabEnv.matchNamedEByPrefix env (Substring.string r)
                    val filteredEs = List.filter (fn (name,_) => name = Substring.string r) (foundRelEs @ foundNamedEs)
                in
                    (case List.map (fn (name, c) => (name, ElabOps.reduceCon env c)) filteredEs of
                         [] => []
                       | (name, (Elab.TRecord (Elab.CRecord (_, fields), _), _)) :: _ =>
                         getCompletionsFromFields env (name ^ ".") (Substring.string str) fields
                       | _ => [])
                end
          | _ => []
    end

(* TODO can we use the real parser to figure out what we're typing (exp, con, field, etc) to predict better? *)
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
            let
                val newtext = List.foldl applyContentChange (#text s) (#contentChanges p)
            in
                insertFileState state fileName { text = newtext
                                               , decls = #decls s
                                               , envBeforeThisModule = #envBeforeThisModule s}
            end
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
                                                 , completionProvider = SOME { triggerCharacters = ["."]}
                                                 , textDocumentSync = { openClose = true
                                                                      , change = 2
                                                                      , save = SOME { includeText = false }
                                                 }}
                                               }
                                       end)
                                      handle (Fail str) => LspSpec.Error (~32602, str)
                    , shutdown = fn () => LspSpec.Error (~32002, "Server not initialized")
                    , textDocument_hover = fn toclient => fn _ => LspSpec.Error (~32002, "Server not initialized")
                    , textDocument_completion = fn _ => LspSpec.Error (~32002, "Server not initialized")
                    }
                | LspSpec.Notification n => ())
          | SOME state => 
            (case requestMessage of
                LspSpec.Notification n =>
                ((LspSpec.handleNotification
                  n
                  { initialized = fn () => ()
                  , textDocument_didOpen = fn toclient => fn p => handleDocumentSavedOrOpened state toclient (#uri (#textDocument p)) (SOME (#text (#textDocument p)))
                  , textDocument_didChange = handleDocumentDidChange state
                  , textDocument_didSave = fn toclient => fn p => handleDocumentSavedOrOpened state toclient (#uri (#textDocument p)) NONE
                  })
                 handle LspError e => handleLspErrorInNotification e
                      | ex => handleLspErrorInNotification (InternalError (General.exnMessage ex)))
              | LspSpec.RequestMessage m => 
                (* TODO should error handling here be inside handleMessage? *)
                ((LspSpec.handleMessage
                  m
                  { initialize = fn _ => LspSpec.Error (~32600, "Server already initialized")
                  , shutdown = fn () => LspSpec.Success ()
                  , textDocument_hover = fn toclient => handleHover state
                  , textDocument_completion = handleCompletion state
                  })
                 handle LspError e => handleLspErrorInRequest (#id m) e
                      | ex => handleLspErrorInRequest (#id m) (InternalError (General.exnMessage ex)))
            ) 
    end

fun startServer () = while true do serverLoop ()
end
