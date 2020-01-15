structure LspSpec = struct 

  datatype lspError = InternalError of string
                    | ServerNotInitialized
  exception LspError of lspError

  fun debug (str: string): unit =
      (TextIO.output (TextIO.stdErr, str ^ "\n\n"); TextIO.flushOut TextIO.stdErr)

  fun trim (s: substring): substring =
      Substring.dropr Char.isSpace (Substring.dropl Char.isSpace s)

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
  type didCloseParams = { textDocument: textDocumentIdentifier }
  fun parseDidCloseParams (params: Json.json): didCloseParams =
      { textDocument = parseTextDocumentIdentifier (FromJson.get "textDocument" params)
      }
  type initializeParams =
       { rootUri: documentUri option
       , initializationOptions: Json.json }
  fun parseInitializeParams (j: Json.json) =
      { rootUri =
        Option.map
          parseDocumentUri
          (FromJson.asOptionalString (FromJson.get "rootUri" j))
      , initializationOptions = FromJson.get "initializationOptions" j
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

  fun matchMessage
          (requestMessage: {id: Json.json, method: string, params: Json.json})
          (handlers: messageHandlers)
      : unit =
    let 
        val result: Json.json result = 
            ((case #method requestMessage of
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
                           Error (~32601, "Method not supported: " ^ method)))
              handle LspError (InternalError str) => Error (~32603, str)
                   | LspError ServerNotInitialized => Error (~32002, "Server not initialized")
                   | ex => Error (~32603, (General.exnMessage ex))
            )
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
       , textDocument_didOpen: (didOpenParams * toclient) -> unit
       , textDocument_didChange: (didChangeParams * toclient) -> unit
       , textDocument_didSave: (didSaveParams * toclient) -> unit
       , textDocument_didClose: (didCloseParams * toclient) -> unit
       }
  fun matchNotification
          (notification: {method: string, params: Json.json})
          (handlers: notificationHandlers)
      =
      (case #method notification of
           "initialized" => (#initialized handlers) ()
         | "textDocument/didOpen" => (#textDocument_didOpen handlers) (parseDidOpenParams (#params notification), toclient)
         | "textDocument/didChange" => (#textDocument_didChange handlers) (parseDidChangeParams (#params notification), toclient)
         | "textDocument/didSave" => (#textDocument_didSave handlers) (parseDidSaveParams (#params notification), toclient)
         | "textDocument/didClose" => (#textDocument_didClose handlers) (parseDidCloseParams (#params notification), toclient)
         | m => debug ("Notification method not supported: " ^ m))
      handle LspError (InternalError str) => showMessage str 1
           | LspError ServerNotInitialized => showMessage "Server not initialized" 1
           | ex => showMessage (General.exnMessage ex) 1
      
end
