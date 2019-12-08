structure Lsp :> LSP = struct 

fun trim (s: substring): substring =
    Substring.dropr
      (fn c => c = #" " orelse c = #"\n" orelse c = #"\r")
      (Substring.dropl (fn c => c = #" " orelse c = #"\n" orelse c = #"\r") s)

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

fun readAllHeaders (l: (string * string) list): (string * string) list = 
    case readHeader () of
        NONE => l
     |  SOME tup => tup :: readAllHeaders l

fun getJsonObjectValue (s: string) (l: (string * Json.json) list): Json.json option =
    case List.find (fn tup => #1 tup = s ) l of
        NONE => NONE
      | SOME tup => SOME (#2 tup)

fun getJsonObjectValue' (s: string) (l: Json.json): Json.json =
    case l of
        Json.Obj l =>
        (case getJsonObjectValue s l of
             NONE => raise Fail ("Failed to find JSON object key " ^ s)
           | SOME v => v)
      | a => raise Fail ("Expected JSON object, got: " ^ Json.print a)

fun parseInt (j: Json.json): int = 
    case j of
        Json.Int i => i
     |  _ => raise Fail ("Expected JSON int, got: " ^ Json.print j)

fun parseString (j: Json.json): string =
    case j of
        Json.String s => s
     |  _ => raise Fail ("Expected JSON string, got: " ^ Json.print j)


fun parseRequest (j: Json.json): {id: Json.json, method: string, params: Json.json} = 
    let 
        val id = getJsonObjectValue' "id" j
        val method = parseString (getJsonObjectValue' "method" j)
        val params = getJsonObjectValue' "params" j
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
        val str = Substring.full (parseString (getJsonObjectValue' "uri" j))
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
    { line = parseInt (getJsonObjectValue' "line" j)
    , character = parseInt (getJsonObjectValue' "character" j)
    }

datatype result = Success of Json.json
                | Error of (int * string)

fun handleHover (params: Json.json): result =
    let
        val textDocument = parseTextDocumentIdentifier (getJsonObjectValue' "textDocument" params)
        val position = parsePosition (getJsonObjectValue' "position" params)
        val answer = ""
    in
        Success (Json.Obj (("contents", Json.String answer) :: []))
    end

fun serverLoop () =            
    let 
        val headers = readAllHeaders []
        val lengthO = List.find (fn (k,v) => k = "Content-Length") headers
        val request = case lengthO of
                          NONE => raise Fail "No header with Content-Length found"
                       |  SOME (k, v) =>
                          case Int.fromString v of
                              NONE => raise Fail ("Couldn't parse content-length from string: " ^ v)
                            | SOME i => TextIO.inputN (TextIO.stdIn, i)
        (* val parsed = Json.parse (Substring.string (Substring.trimr 1 (Substring.full request))) (* Trimming last newline *) *)
        val parsed = Json.parse request
        val requestMessage = parseRequest parsed
        fun fail (err: (int * string)) =
            Json.print (Json.Obj (("id", #id requestMessage)
                                  :: ("error", Json.Obj (("code", Json.Int (#1 err))
                                                         :: ("message", Json.String (#2 err))
                                                         :: []))
                                  :: []
                       ))
        val result: result = 
            case #method requestMessage of
                "initialize" => Success (Json.Obj (("capabilities", Json.Obj (("hoverProvider", Json.Bool true) :: [])) :: []))
              | "textDocument/hover" => handleHover (#params requestMessage)
              | "shutdown" => Success (Json.Null)
              | "exit" => OS.Process.exit OS.Process.success
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

fun startServer () =
    while (1 < 2) do
          serverLoop ()
end
