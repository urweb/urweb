structure C = Compiler

fun trim (s: substring): substring =
    Substring.dropr
        (fn c => c = #" " orelse c = #"\n" orelse c = #"\r")
        (Substring.dropl (fn c => c = #" " orelse c = #"\n" orelse c = #"\r") s)

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
          val (path, rest) = Substring.splitl (fn c => c <> #"?" orelse c <> #"#") (Substring.triml 1 rest (* / *))
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
  type context = { showMessage: string -> int -> unit}
  type messageHandlers =
       { initialize: initializeParams -> initializeResponse result
       , shutdown: unit -> unit result
       , textDocument_hover: context -> hoverReq -> hoverResp result
       }
      
  fun handleMessage
          (requestMessage: {id: Json.json, method: string, params: Json.json})
          (handlers: messageHandlers)
      : unit =
    let 
        fun showMessage str typ =
            TextIO.print (Json.print (Json.Obj [ ("method", Json.String "window/showMessage")
                                               , ("params", Json.Obj [ ("type", Json.Int typ)
                                                                     , ("message", Json.String str)])
                         ]));
        
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
                         {showMessage = showMessage}
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
       , textDocument_didOpen: didOpenParams -> unit
       , textDocument_didChange: didChangeParams -> unit
       , textDocument_didSave: didSaveParams -> unit
       }
  fun handleNotification
          (notification: {method: string, params: Json.json})
          (handlers: notificationHandlers)
      = case #method notification of
            "initialized" => (#initialized handlers) ()
          | "textDocument/didOpen" => (#textDocument_didOpen handlers) (parseDidOpenParams (#params notification))
          | "textDocument/didChange" => (#textDocument_didChange handlers) (parseDidChangeParams (#params notification))
          | "textDocument/didSave" => (#textDocument_didSave handlers) (parseDidSaveParams (#params notification))
          | m => (TextIO.output ( TextIO.stdErr, "Notification method not supported: " ^ m);
                  TextIO.flushOut TextIO.stdErr)
      
end

structure Lsp :> LSP = struct 

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
                      | one :: [] => one
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
                        else ([], env)
    in
        env'
    end

fun calculateFileState (state: state) (fileName: string): fileState =
    let
        (* TODO Optim: cache parsed urp file? *)
        val () = if (OS.Path.ext fileName = SOME "ur")
                 then ()
                 else raise Fail ("Can only handle .ur files for now")
        val () = Elaborate.unifyMore := true
        val job = valOf (C.run (C.transform C.parseUrp "parseUrp") (#urpPath state))
        fun entryInUrpToFileName (entry: string) (ext: string) = (#urpPath state) ^ "/" ^ entry ^ ext
        val modulesBeforeAndAfterThisFile =
            List.partition (fn entry => entryInUrpToFileName entry ".ur" = fileName) (#sources job)
        val () = case #2 modulesBeforeAndAfterThisFile of 
                     [] =>
                     (* Module we're handling should always be in here *)
                     raise Fail ("Couldn't find file " ^ fileName ^ " referenced in .urp file at " ^ (#urpPath state))
                   | _ => ()
        val parsedUrss = List.map (fn entry =>
                                      let
                                          val fileName = entryInUrpToFileName entry ".urs"
                                      in
                                          { fileName = fileName
                                          , parsed =
                                            if OS.FileSys.access (fileName, [])
                                            then raise (Fail ("Couldn't find an .urs file for " ^ fileName))
                                            else valOf (C.run (C.transform C.parseUrs "parseUrs") fileName)}
                                      end)
                                  (#1 modulesBeforeAndAfterThisFile)
        val parsedBasisUrs = valOf (C.run (C.transform C.parseUrs "parseUrs") (Settings.libFile "basis.urs"))
        val parsedTopUrs = valOf (C.run (C.transform C.parseUrs "parseUrs") (Settings.libFile "top.urs"))
        val envWithStdLib =
            addSgnToEnv
                (addSgnToEnv ElabEnv.empty parsedBasisUrs (Settings.libFile "basis.urs") true)
                parsedTopUrs (Settings.libFile "top.urs") true
        val envBeforeThisFile = List.foldl (fn (sgn, env) => addSgnToEnv env (#parsed sgn) (#fileName sgn) false) envWithStdLib parsedUrss
        val (parsedUr: Source.decl list) =
            valOf (C.run (C.transform C.parseUr "parseUr") fileName)
        val (parsedUrs: (Source.sgn_item list) option) =
            if OS.FileSys.access (fileName ^ "s", []) then
                SOME (valOf (C.run (C.transform C.parseUrs "parseUrs") (fileName ^ "s")))
            else
                NONE
        val (str, sgn', gs) =
            Elaborate.elabStr
                (envBeforeThisFile, Disjoint.empty)
                (Source.StrConst parsedUr, {file = fileName, first = ErrorMsg.dummyPos, last = ErrorMsg.dummyPos})

        (* TODO definitily not sure about this one, just copied from "top" processing *)
        val () = case gs of
                     [] => ()
                   | _ => app (fn Elaborate.Disjoint (loc, env, denv, c1, c2) =>
                                  (case Disjoint.prove env denv (c1, c2, loc) of
                                       [] => ()
                                     | _ =>
                                       (Print.prefaces "Unresolved constraint in top.ur"
                                                       [("loc", Print.PD.string (ErrorMsg.spanToString loc)),
                                                        ("c1", ElabPrint.p_con env c1),
                                                        ("c2", ElabPrint.p_con env c2)];
                                        raise Fail "Unresolved constraint in top.ur"))
                              | Elaborate.TypeClass (env, c, r, loc) =>
                                ()
                                (* let *)
                                (*     val c = normClassKey env c *)
                                (* in *)
                                (*     case resolveClass env c of *)
                                (*         SOME e => r := SOME e *)
                                (*       | NONE => expError env (Unresolvable (loc, c)) *)
                              (* end *)
                              ) gs
        val (sgn, gs) = Elaborate.elabSgn
                            (envBeforeThisFile, Disjoint.empty)
                            ( Source.SgnConst (case parsedUrs of NONE => [] | SOME a => a)
                            , {file = fileName ^ "s", first = ErrorMsg.dummyPos, last = ErrorMsg.dummyPos})
        val () = case gs of [] => () | _ => raise Fail ("Unresolved disjointness constraints in " ^ fileName) (* TODO not sure? *)
        val () = Elaborate.subSgn envBeforeThisFile ErrorMsg.dummySpan sgn' sgn
    in
        { envOfPreviousModules = envBeforeThisFile
        , decls = case str of
                      (Elab.StrConst decls, _) => decls
                    | _ => raise Fail ("Impossible: Source.StrConst did not become Elab.StrConst after elaboration")
        }
    end

fun serverLoop () =            
    let 
        val requestMessage =
            LspSpec.readRequestFromStdIO ()
            handle ex => (TextIO.output (TextIO.stdErr, General.exnMessage ex ^ "\n"); TextIO.flushOut TextIO.stdErr ; raise ex)
        val state = !stateRef
    in
        case state of
            NONE =>
            (case requestMessage of
                  LspSpec.RequestMessage m =>
                  LspSpec.handleMessage
                    m
                    { initialize = fn _ => 
                                      let val st = initState (LspSpec.parseInitializeParams (#params m))
                                      in
                                          stateRef := SOME st;
                                          LspSpec.Success
                                            { capabilities =
                                                { hoverProvider = true
                                                , textDocumentSync = { openClose = true
                                                                      , save = SOME { includeText = false }
                                                                      }}
                                            }
                                      end
                                      handle (Fail str) => LspSpec.Error (~32602, str)
                    , shutdown = fn () => LspSpec.Error (~32002, "Server not initialized")
                    , textDocument_hover = fn ctx => fn _ => LspSpec.Error (~32002, "Server not initialized")
                    }
                | LspSpec.Notification n => ())
          | SOME state => 
            (case requestMessage of
                LspSpec.Notification n =>
                LspSpec.handleNotification
                    n
                    { initialized = fn () => ()
                    , textDocument_didOpen = fn didOpenParams =>
                                                let 
                                                    val path = #path (#uri (#textDocument didOpenParams))
                                                    val fileState = calculateFileState state path
                                                in
                                                    stateRef := SOME { urpPath = #urpPath state
                                                                     , fileStates = SM.insert ( #fileStates state
                                                                                              , path
                                                                                              , fileState)
                                                                     }
                                                end
                    , textDocument_didChange = fn didChangeParams => ()
                    , textDocument_didSave = fn didSaveParams => 
                                                let 
                                                    val path = #path (#uri (#textDocument didSaveParams))
                                                    val fileState = calculateFileState state path
                                                in
                                                    stateRef := SOME { urpPath = #urpPath state
                                                                     , fileStates = SM.insert ( #fileStates state
                                                                                              , path
                                                                                              , fileState)
                                                                     }
                                                end
                    }
              | LspSpec.RequestMessage m => 
                LspSpec.handleMessage
                    m
                    { initialize = fn _ => LspSpec.Error (~32600, "Server already initialized")
                    , shutdown = fn () => LspSpec.Success ()
                    , textDocument_hover = fn ctx => fn _ => LspSpec.Success NONE
                    }
            ) handle ex => (TextIO.output (TextIO.stdErr, General.exnMessage ex ^ "\n"); TextIO.flushOut TextIO.stdErr ; raise ex)
    end

fun startServer () = while true do serverLoop ()
end
