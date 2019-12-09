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

  type textDocumentIdentifier =
       { scheme: string
       , authority: string
       , path: string
       , query: string
       , fragment: string
       }
  fun parseTextDocumentIdentifier (j: Json.json): textDocumentIdentifier = 
      let
          val str = Substring.full (FromJson.asString (FromJson.get "uri" j))
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

  fun parseHoverReq (params: Json.json): { textDocument: textDocumentIdentifier , position: position } =
      { textDocument = parseTextDocumentIdentifier (FromJson.get "textDocument" params)
      , position = parsePosition (FromJson.get "position" params)
      }

  fun printHoverResponse (resp: {contents: string} option): Json.json =
      case resp of
          NONE => Json.Null
        | SOME obj => Json.Obj [("contents", Json.String (#contents obj))]

  datatype 'a result =
           Success of 'a
         | Error of (int * string)

  fun mapResult (f: 'a -> 'b) (a: 'a result): 'b result =
      case a of
          Success contents => Success (f contents)
        | Error e => Error e
  type messageHandlers =
       { initialize: unit -> { capabilities: {hoverProvider: bool}} result
       , shutdown: unit -> unit result
       , textDocument_hover:
         { showMessage: string -> int -> unit}
         -> { textDocument: textDocumentIdentifier
            , position: position }
         ->  ({contents: string} option) result
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
                    (fn res => Json.Obj [("capabilities", Json.Obj [("hoverProvider", Json.Bool (#hoverProvider (#capabilities res)))])])
                    ((#initialize handlers) ())
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
       }
  fun handleNotification
          (notification: {method: string, params: Json.json})
          (handlers: notificationHandlers)
      = case #method notification of
            "initialized" => (#initialized handlers) ()
          | m => (TextIO.output ( TextIO.stdErr, "Notification method not supported: " ^ m);
                  TextIO.flushOut TextIO.stdErr)

      
end

structure Lsp :> LSP = struct 

fun serverLoop () =            
    let 
        val requestMessage =
            LspSpec.readRequestFromStdIO ()
            handle ex => (TextIO.output (TextIO.stdErr, General.exnMessage ex ^ "\n"); TextIO.flushOut TextIO.stdErr ; raise ex)
    in
        (case requestMessage of
             LspSpec.Notification n =>
             ((*  TextIO.output (TextIO.stdErr, "Handling notification: " ^ #method n ^ "\n"); *)
              (* TextIO.flushOut TextIO.stdErr; *)
              LspSpec.handleNotification
                  n
                  { initialized = fn () => ()
             })
           | LspSpec.RequestMessage m => 
             ((* TextIO.output (TextIO.stdErr, "Handling message: " ^ #method m ^ "\n"); *)
              (* TextIO.flushOut TextIO.stdErr; *)
              LspSpec.handleMessage
                  m
                  { initialize = fn () => LspSpec.Success {capabilities = {hoverProvider = true}}
                  , shutdown = fn () => LspSpec.Success ()
                  , textDocument_hover =
                    fn ctx =>
                       fn _ => LspSpec.Success NONE
                  }
             )
        ) handle ex => (TextIO.output (TextIO.stdErr, General.exnMessage ex ^ "\n"); TextIO.flushOut TextIO.stdErr ; raise ex)
    end

fun startServer () = while true do serverLoop ()
end
