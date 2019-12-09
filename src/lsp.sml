
fun trim (s: substring): substring =
    Substring.dropr
        (fn c => c = #" " orelse c = #"\n" orelse c = #"\r")
        (Substring.dropl (fn c => c = #" " orelse c = #"\n" orelse c = #"\r") s)

structure FromJson = struct 
fun get (s: string) (l: Json.json): Json.json =
    let 
        fun getJsonObjectValue (s: string) (l: (string * Json.json) list): Json.json option =
            case List.find (fn tup => #1 tup = s ) l of
                NONE => NONE
              | SOME tup => SOME (#2 tup)
    in 
        case l of
            Json.Obj l =>
            (case getJsonObjectValue s l of
                 NONE => raise Fail ("Failed to find JSON object key " ^ s)
               | SOME v => v)
          | a => raise Fail ("Expected JSON object, got: " ^ Json.print a)
    end

fun asInt (j: Json.json): int = 
    case j of
        Json.Int i => i
     |  _ => raise Fail ("Expected JSON int, got: " ^ Json.print j)

fun asString (j: Json.json): string =
    case j of
        Json.String s => s
     |  _ => raise Fail ("Expected JSON string, got: " ^ Json.print j)

end

(* signature LSPSPEC = sig *)
(*     type textDocumentIdentifier = *)
(*          { scheme: string *)
(*          , authority: string *)
(*          , path: string *)
(*          , query: string *)
(*          , fragment: string *)
(*          } *)
(*     type position = { line: int *)
(*                     , character: int  *)
(*                     } *)
(*     val readRequestFromStdIO: () -> {id: Json.json, method: string, params: Json.json} *)
(*     val parseRequest: {id: Json.json, method: string, params: Json.json} -> request *)
(*     datatype request =  *)
(* end *)

                        

structure LspSpec (* :> LSPSPEC *) = struct 
  fun readHeader (): (string * string) option =
      let 
          val line = TextIO.inputLine TextIO.stdIn
      in 
          case line of
              NONE => OS.Process.exit OS.Process.success
          |  SOME str =>
              let 
                  val (key, value) = Substring.splitl (fn c => c <> #":") (Substring.full str)
              in 
                  if Substring.isEmpty (trim value)
                  then NONE
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

  fun parseBasicRequest (j: Json.json): {id: Json.json, method: string, params: Json.json} = 
      let 
          val id = FromJson.get "id" j
          val method = FromJson.asString (FromJson.get "method" j)
          val params = FromJson.get "params" j
      in 
          {id = id, method = method, params = params}
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


  fun readRequestFromStdIO (): {id: Json.json, method: string, params: Json.json} =
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
          parseBasicRequest parsed
      end

  fun parseHoverReq (params: Json.json): { textDocument: textDocumentIdentifier , position: position } =
      { textDocument = parseTextDocumentIdentifier (FromJson.get "textDocument" params)
      , position = parsePosition (FromJson.get "position" params)
      }

  fun printHoverResponse (resp: {contents: string}): Json.json =
      Json.Obj (("contents", Json.String (#contents resp)) :: [])

  datatype 'a result =
           Success of 'a
         | Error of (int * string)

  fun mapResult (f: 'a -> 'b) (a: 'a result): 'b result =
      case a of
          Success contents => Success (f contents)
        | Error e => Error e
  type handlers =
       { initialize: unit -> { capabilities: {hoverProvider: bool}} result
       , shutdown: unit -> unit result
       , textDocument_hover: { textDocument: textDocumentIdentifier
                             , position: position }
                             ->  {contents: string} result
       }
      
  fun handleRequest
          (requestMessage: {id: Json.json, method: string, params: Json.json})
          (handlers: handlers)
      : unit =
    let 
        val result: Json.json result = 
            case #method requestMessage of
                "initialize" =>
                mapResult
                    (fn res => Json.Obj (("capabilities", Json.Obj (("hoverProvider", Json.Bool (#hoverProvider (#capabilities res)))
                                                                    :: []))
                                         :: []))
                    ((#initialize handlers) ())
              | "textDocument/hover" =>
                mapResult
                    printHoverResponse
                    ((#textDocument_hover handlers)
                         (parseHoverReq (#params requestMessage)))
              | "shutdown" =>
                mapResult
                    (fn () => Json.Null)
                    ((#shutdown handlers) ())
              | "exit" =>
                OS.Process.exit OS.Process.success
              | method => Error (~32601, "Method not supported: " ^ method)
    in 
        case result of
            Success j => TextIO.output (TextIO.stdOut,
                                        Json.print (Json.Obj (("id", #id requestMessage)
                                                              :: ("result", j)
                                                              :: [])))
          | Error (i, err) => 
            TextIO.output (TextIO.stdOut, 
                           Json.print (Json.Obj (("id", #id requestMessage)
                                                 :: ("error", Json.Obj (("code", Json.Int i)
                                                                        :: ("message", Json.String err)
                                                                        :: []))
                                                 :: []
                          )))
    end
      
end

structure Lsp :> LSP = struct 

fun serverLoop () =            
    let 
        val requestMessage = LspSpec.readRequestFromStdIO ()
    in
        LspSpec.handleRequest
            requestMessage
            { initialize = fn () => LspSpec.Success {capabilities = {hoverProvider = true}}
            , shutdown = fn () => LspSpec.Success ()
            , textDocument_hover = fn _ => LspSpec.Success {contents = ""}
            }
    end

fun startServer () = while true do serverLoop ()
end
