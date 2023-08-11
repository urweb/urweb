con json a = {ToJson : a -> string,
              FromJson : string -> a * string,
              ToYaml : int (* starting indent level *) -> a -> string,
              FromYaml : bool (* comes immediately after list bullet? *) -> int (* starting indent level *) -> string -> a * string}

fun mkJson [a] (x : {ToJson : a -> string,
                     FromJson : string -> a * string}) = x ++ {ToYaml = fn _ _ => error <xml>No YAML support</xml>,
                                                               FromYaml = fn _ _ _ => error <xml>No YAML support</xml>}

fun skipSpaces s =
    if s = "" then
        ""
    else if Char.isSpace (String.sub s 0) then
        skipSpaces (String.suffix s 1)
    else
        s

fun skipRealSpaces s =
    if s = "" then
        ""
    else if String.sub s 0 = #" " then
        skipRealSpaces (String.suffix s 1)
    else
        s

fun toJson [a] (j : json a) : a -> string = j.ToJson
fun fromJson' [a] (j : json a) : string -> a * string = j.FromJson

fun fromJson [a] (j : json a) (s : string) : a =
    let
        val (v, s') = j.FromJson (skipSpaces s)
    in
        if String.all Char.isSpace s' then
            v
        else
            error <xml>Extra content at end of JSON record: {[s']}</xml>
    end

fun toYaml [a] (j : json a) : a -> string = j.ToYaml 0
fun fromYaml [a] (j : json a) (s : string) : a =
    let
        val (v, s') = j.FromYaml False 0 (skipSpaces s)
    in
        if String.all Char.isSpace s' then
            v
        else
            error <xml>Extra content at end of YAML record: {[s']}</xml>
    end

fun escape s =
    let
        fun esc s =
            case s of
                "" => "\""
              | _ =>
                let
                    val ch = String.sub s 0
                in
		    (case ch of
			 #"\n" => "\\n"
		       | #"\r" => "\\r"
		       | #"\t" => "\\t"
		       | #"\"" => "\\\""
		       | #"\\" => "\\\\"
		       | x => String.str ch
		    ) ^ esc (String.suffix s 1)
                end
    in
        "\"" ^ esc s
    end

fun unhex ch =
    if Char.isDigit ch then
        Char.toInt ch - Char.toInt #"0"
    else if Char.isXdigit ch then
        if Char.isUpper ch then
            10 + (Char.toInt ch - Char.toInt #"A")
        else
            10 + (Char.toInt ch - Char.toInt #"a")
    else
        error <xml>Invalid hexadecimal digit "{[ch]}"</xml>

fun unescape s =
    let
        fun findEnd endChar i s =
            if s = "" then
                error <xml>JSON unescape: string ends before quote: {[s]}</xml>
            else
                let
                    val ch = String.sub s 0
                in
                    case ch of
                        #"\\" =>
                        if not (strlenGe s 2) then
                            error <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                        else if String.sub s 1 = #"u" then
                            if not (strlenGe s 6) then
                                error <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                            else
                                findEnd endChar (i+6) (String.suffix s 6)
                        else
                            findEnd endChar (i+2) (String.suffix s 2)
                      | _ =>
                        if ch = endChar then
                            i
                        else
                            findEnd endChar (i+1) (String.suffix s 1)
                end

        fun unesc last i s =
            if i >= last then
                ""
            else
                let
                    val ch = String.sub s 0
                in
                    case ch of
                        #"\\" =>
                        if not (strlenGe s 2) then
                            error <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                        else if String.sub s 1 = #"u" then
                            if not (strlenGe s 6) then
                                error <xml>JSON unescape: Unicode ends early</xml>
                            else
                                let
                                    val n =
                                        unhex (String.sub s 2) * (256*16)
                                        + unhex (String.sub s 3) * 256
                                        + unhex (String.sub s 4) * 16
                                        + unhex (String.sub s 5)
                                in
                                    ofUnicode n ^ unesc last (i+6) (String.suffix s 6)
                                end
                        else
			    (case String.sub s 1 of
				 #"n" => "\n"
                               | #"r" => "\r"
                               | #"t" => "\t"
                               | #"\"" => "\""
                               | #"'" => "'"
			       | #"\\" => "\\"
			       | #"/" => "/"
                               | #" " => " "
                               | x => error <xml>JSON unescape: Bad escape char: {[x]}</xml>)
			    ^
			    unesc last (i+2) (String.suffix s 2)
                      | _ => String.str ch ^ unesc last (i+1) (String.suffix s 1)
                end
    in
        if s = "" || (String.sub s 0 <> #"\"" && String.sub s 0 <> #"'") then
            error <xml>JSON unescape: String doesn't start with quote: {[s]}</xml>
        else
            let
                val last = findEnd (String.sub s 0) 1 (String.suffix s 1)
            in
                (unesc last 1 (String.suffix s 1), String.suffix s (last+1))
            end
    end

fun readYamlLine (minIndent : option int) (* set if we come right after a list bullet and should fake some indent *)
                 (s : string) : int (* indent level *) * string (* remaining code *) =
    let
        fun read s acc =
            if s = "" then
                (acc, "")
            else
                let
                    val ch = String.sub s 0
                in
                    if ch = #"\n" || ch = #"\r" then
                        read (String.suffix s 1) 0
                    else if ch = #"#" then
                        (* Comment *)
                        case String.seek s #"\n" of
                            None => (acc, s)
                          | Some rest => read rest 0
                    else if ch = #" " then
                        read (String.suffix s 1) (acc + 1)
                    else
                        (acc, s)
                end

        val (i, s) = read s 0
    in
        (case minIndent of
             None => i
           | Some i' => max i' i, s)
    end

datatype block_style = ToSpaces | KeepNewlines
datatype chomp_style = SingleNewline | NoNewline | AllNewlines

fun newlines i =
    if i <= 0 then
        ""
    else
        newlines (i-1) ^ "\n"

fun readMultilineString block chomp i s =
    let
        fun read s acc onl (* length of longest suffix of acc that is only newline characters *) =
            let
                val (i', s') = readYamlLine None s
            in
                if i' < i then
                    (case chomp of
                         SingleNewline => String.substring acc {Start = 0, Len = String.length acc - onl} ^ "\n"
                       | NoNewline => String.substring acc {Start = 0, Len = String.length acc - onl}
                       | AllNewlines =>
                         case block of
                             KeepNewlines => acc
                           | ToSpaces => String.substring acc {Start = 0, Len = String.length acc - onl} ^ newlines onl, s)
                else
                    case String.split s' #"\n" of
                        None => error <xml>Multiline YAML string ends without newline.</xml>
                      | Some (line, s') => read s' (acc ^ line ^ (case block of ToSpaces => " " | KeepNewlines => "\n"))
                                                (if String.all (fn ch => ch = #"\r") line then
                                                     onl + String.length line + 1
                                                 else if line <> "" && String.sub line (String.length line - 1) = #"\r" then
                                                     2
                                                 else
                                                     1)
            end
    in
        case String.seek s #"\n" of
            None => error <xml>Multiline YAML string ends too early.</xml>
          | Some s' => read s' "" 0
    end

fun readMultiline block i s =
    let
        val s = String.suffix s 1
        val (chomp, s) = if s = "" then
                             error <xml>Multiline YAML string ends too early.</xml>
                         else if String.sub s 0 = #"-" then
                             (NoNewline, String.suffix s 1)
                         else if String.sub s 0 = #"+" then
                             (AllNewlines, String.suffix s 1)
                         else
                             (SingleNewline, s)
    in
        readMultilineString block chomp i s
    end

fun stringIn i s =
    if s = "" then
        ("", "")
    else if String.sub s 0 = #"\"" || String.sub s 0 = #"'" then
        unescape s
    else if String.sub s 0 = #">" then
        readMultiline ToSpaces i s
    else if String.sub s 0 = #"|" then
        readMultiline KeepNewlines i s
   else case String.msplit {Haystack = s, Needle = "\r\n"} of
             None => (s, "")
           | Some (v, _, rest) => (v, rest)

val json_string = {ToJson = escape,
                   FromJson = unescape,
                   ToYaml = fn _ => escape,
                   FromYaml = fn _ => stringIn}

fun rfc3339_out s =
    let
        val out1 = timef "%Y-%m-%dT%H:%M:%S%z" s
        val len = String.length out1
    in
        if len < 2 then
            error <xml>timef output too short</xml>
        else
            String.substring out1 {Start = 0, Len = len - 2} ^ ":"
            ^ String.suffix out1 (len - 2)
    end

fun rfc3339_in s =
    case String.split s #"T" of
        None => error <xml>Invalid RFC 3339 string "{[s]}"</xml>
      | Some (date, time) =>
        case String.msplit {Haystack = time, Needle = "Z+-"} of
            None => error <xml>Invalid RFC 3339 string "{[s]}"</xml>
          | Some (time, sep, rest) =>
            let
                val time = case String.split time #"." of
                               None => time
                             | Some (time, _) => time

                val t = case readUtc (date ^ " " ^ time) of
                            None => error <xml>Invalid RFC 3339 string "{[s]}"</xml>
                          | Some t => t

                fun withOffset multiplier =
                    case String.split rest #":" of
                        None => error <xml>Invalid RFC 3339 string "{[s]}"</xml>
                      | Some (h, m) =>
                        case (read h, read m) of
                            (Some h, Some m) => addSeconds t (multiplier * 60 * (60 * h + m))
                          | _ => error <xml>Invalid RFC 3339 string "{[s]}"</xml>
            in
                case sep of
                    #"Z" => t
                  | #"+" => withOffset (-1)
                  | #"-" => withOffset 1
                  | _ => error <xml>msplit returns impossible separator</xml>
            end

fun timeOut s =
    let
        val (v, s') = unescape s
    in
        (rfc3339_in v, s')
    end
val json_time = {ToJson = fn tm => escape (rfc3339_out tm),
                 FromJson = timeOut,
                 ToYaml = fn _ tm => escape (rfc3339_out tm),
                 FromYaml = fn _ _ => timeOut}

fun numIn [a] (_ : read a) s : a * string =
    let
        val len = String.length s

        fun findEnd i s =
            if s = "" then
                i
            else
                let
                    val ch = String.sub s 0
                in
                    if Char.isDigit ch || ch = #"-" || ch = #"." || ch = #"E" || ch = #"e" then
                        findEnd (i+1) (String.suffix s 1)
                    else
                        i
                end
    in
        if s <> "" && String.sub s 0 = #"\"" then
            let
                val last = findEnd 1 (String.suffix s 1)
                val rest = String.suffix s last
            in
                if rest <> "" && String.sub rest 0 = #"\"" then
                    (readError (String.substring s {Start = 1, Len = last-1}),
                     String.suffix rest 1)
                else
                    error <xml>Unbalanced quotes for JSON number {[s]}</xml>
            end
        else if s <> "" && String.sub s 0 = #"'" then
            let
                val last = findEnd 1 (String.suffix s 1)
                val rest = String.suffix s last
            in
                if rest <> "" && String.sub rest 0 = #"'" then
                    (readError (String.substring s {Start = 1, Len = last-1}),
                     String.suffix rest 1)
                else
                    error <xml>Unbalanced quotes for JSON number {[s]}</xml>
            end
        else
            let
                val last = findEnd 0 s
            in
                (readError (String.substring s {Start = 0, Len = last}), String.suffix s last)
            end
    end

fun json_num [a] (_ : show a) (_ : read a) : json a = {ToJson = show,
                                                       FromJson = numIn,
                                                       ToYaml = fn _ => show,
                                                       FromYaml = fn _ _ => numIn}

val json_int = json_num
val json_float = json_num

val json_bool = {ToJson = fn b => if b then "true" else "false",
                 FromJson = fn s => if String.isPrefix {Full = s, Prefix = "true"} then
                                        (True, String.suffix s 4)
                                    else if String.isPrefix {Full = s, Prefix = "false"} then
                                        (False, String.suffix s 5)
                                    else
                                        error <xml>JSON: bad boolean string: {[s]}</xml>,
                 ToYaml = fn _ b => if b then "True" else "False",
                 FromYaml = fn _ _ s =>
                               case String.msplit {Haystack = s, Needle = " \r\n"} of
                                   None => error <xml>No space after Boolean in YAML</xml>
                                 | Some (s', _, _) =>
                                   let
                                       val s' = String.mp Char.toLower s'

                                       val v = if s' = "true" || s' = "on" || s' = "yes" then
                                                   True
                                               else if s' = "false" || s' = "off" || s' = "no" then
                                                   False
                                               else
                                                   error <xml>Invalid YAML Boolean: {[s']}</xml>
                                   in
                                       (v, String.suffix s (String.length s'))
                                   end}

fun json_option [a] (j : json a) : json (option a) =
    {ToJson = fn v => case v of
                          None => "null"
                        | Some v => j.ToJson v,
     FromJson = fn s => if String.isPrefix {Full = s, Prefix = "null"} then
                            (None, String.suffix s 4)
                        else
                            let
                                val (v, s') = j.FromJson s
                            in
                                (Some v, s')
                            end,
     ToYaml = fn i v => case v of
                            None => "null"
                          | Some v => j.ToYaml i v,
     FromYaml = fn b i s => if String.isPrefix {Full = s, Prefix = "null"} then
                                (None, String.suffix s 4)
                            else
                                let
                                    val (v, s') = j.FromYaml b i s
                                in
                                    (Some v, s')
                                end
    }

fun indent i =
    if i <= 0 then
        ""
    else
        " " ^ indent (i - 1)

fun truncAtNewline s =
    case String.split s #"\n" of
        None => s
      | Some (s', _) => s'

fun json_list [a] (j : json a) : json (list a) =
    let
        fun toJ' (ls : list a) : string =
            case ls of
                [] => ""
              | x :: ls => "," ^ toJson j x ^ toJ' ls

        fun toJ (x : list a) : string =
            case x of
                [] => "[]"
              | x :: [] => "[" ^ toJson j x ^ "]"
              | x :: ls => "[" ^ toJson j x ^ toJ' ls ^ "]"

        fun fromJ (s : string) : list a * string =
            let
                fun fromJ' (s : string) : list a * string =
                    if s = "" then
                        error <xml>JSON list doesn't end with ']'</xml>
                    else
                        let
                            val ch = String.sub s 0
                        in
                            case ch of
                                #"]" => ([], String.suffix s 1)
                              | _ =>
                                let
                                    val (x, s') = j.FromJson s
                                    val s' = skipSpaces s'
                                    val s' = if s' = "" then
                                                 error <xml>JSON list doesn't end with ']'</xml>
                                             else if String.sub s' 0 = #"," then
                                                 skipSpaces (String.suffix s' 1)
                                             else
                                                 s'

                                    val (ls, s'') = fromJ' s'
                                in
                                    (x :: ls, s'')
                                end
                        end
            in
                if String.length s = 0 || String.sub s 0 <> #"[" then
                    error <xml>JSON list doesn't start with '[': {[s]}</xml>
                else
                    fromJ' (skipSpaces (String.suffix s 1))
            end

        fun toY (i : int) (ls : list a) : string =
            case ls of
                [] => ""
              | x :: ls' => indent (i + 1) ^ "- " ^ j.ToYaml (i + 3) x ^ toY i ls'

        fun fromY (b : bool) (i : int) (s : string) : list a * string =
            let
                val (i', s') = readYamlLine (if b then Some i else None) s
            in
                if i' < i || s' = "" then
                    ([], s)
                else if String.sub s' 0 = #"-" then
                    let
                        val s' = String.suffix s' 1
                        val (s', i') = if s' <> "" && String.sub s' 0 = #" " then
                                           (String.suffix s' 1, i' + 2)
                                       else
                                           (s', i' + 1)
                        val (v, s) = j.FromYaml True i' s'
                        val (ls, s) = fromY False i s
                    in
                        (v :: ls, s)
                    end
                else
                    error <xml>YAML list contains weird delimiter.</xml>
            end
    in
        {ToJson = toJ,
         FromJson = fromJ,
         ToYaml = toY,
         FromYaml = fn b i s => if String.isPrefix {Full = s, Prefix = "[]"} then
                                    ([], String.suffix s 2)
                                else
                                    fromY b i s}
    end

fun skipOne s =
    let
        fun skipStringLiteral s delimiter =
            if s = "" then
                s
            else
                let
                    val ch = String.sub s 0
                    val rest = String.suffix s 1
                in
                    if ch = delimiter then
                        rest
                    else if ch = #"\\" then
                        if rest <> "" then
                            skipStringLiteral (String.suffix s 2) delimiter
                        else
                            ""
                    else
                        skipStringLiteral rest delimiter
                end

        fun skipOne s brace bracket =
            if s = "" then
                s
            else
                let
                    val ch = String.sub s 0
                    val rest = String.suffix s 1
                in
                    case ch of
                        #"\"" => skipOne (skipStringLiteral rest #"\"") brace bracket
                      | #"'" => skipOne (skipStringLiteral rest #"'") brace bracket
                      | #"{" => skipOne rest (brace + 1) bracket
                      | #"}" => if brace = 0 then
                                    s
                                else
                                    skipOne rest (brace - 1) bracket

                      | #"[" => skipOne rest brace (bracket + 1)
                      | #"]" =>
                        if bracket = 0 then
                            s
                        else
                            skipOne rest brace (bracket - 1)

                      | #"," =>
                        if brace = 0 && bracket = 0 then
                            s
                        else
                            skipOne rest brace bracket

                      | _ => skipOne rest brace bracket
                end
    in
        skipOne s 0 0
    end

fun firstTen s =
    if String.lengthGe s 10 then String.substring s {Start = 0, Len = 10} else s

fun skipUntilIndentLowEnough target s =
    let
        fun skip s =
            let
                val (i, s') = readYamlLine None s
            in
                if i <= target then
                    s
                else
                    case String.seek s' #"\n" of
                        None => s'
                      | Some s' => skip s'
            end

        (* Don't look for indentation in the first line,
         * as indeed it may not come at an actual line beginning. *)
        val s' = case String.seek s #"\n" of
                     None => s
                   | Some s => skip s
    in
        s'
    end

fun json_record_withOptional [ts ::: {Type}] [ots ::: {Type}] [ts ~ ots]
                             (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts))
                             (ofl : folder ots) (ojss : $(map json ots)) (onames : $(map (fn _ => string) ots)): json $(ts ++ map option ots) =
    {ToJson = fn r =>
                 let
                     val withRequired =
                         @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                              escape name ^ ":" ^ j.ToJson v ^ (case acc of
                                                                    "" => ""
                                                                  | acc => "," ^ acc))
                          "" fl jss names (r --- _)

                     val withOptional =
                         @foldR3 [json] [fn _ => string] [option] [fn _ => string]
                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                              case v of
                                  None => acc
                                | Some v =>
                                  escape name ^ ":" ^ j.ToJson v ^ (case acc of
                                                                        "" => ""
                                                                      | acc => "," ^ acc))
                          withRequired ofl ojss onames (r --- _)
                 in
                     "{" ^ withOptional ^ "}"
                 end,
     FromJson = fn s =>
                   let
                       fun fromJ s (r : $(map option (ts ++ ots))) : $(map option (ts ++ ots)) * string =
                           if s = "" then
                               error <xml>JSON object doesn't end in brace</xml>
                           else if String.sub s 0 = #"}" then
                               (r, String.suffix s 1)
                           else let
                                   val (name, s') = unescape s
                                   val s' = skipSpaces s'
                                   val s' = if s' = "" || String.sub s' 0 <> #":" then
                                                error <xml>No colon after JSON object field name</xml>
                                            else
                                                skipSpaces (String.suffix s' 1)

                                   val (r, s') = @foldR3 [fn _ => bool] [json] [fn _ => string] [fn ts => $(map option ts) -> $(map option ts) * string]
                                                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] skipNull (j : json t) name' acc r =>
                                                      if name = name' then
                                                          if skipNull
                                                             && String.lengthGe s' 5
                                                             && String.isPrefix {Prefix = "null", Full = s'}
                                                             && (Char.isSpace (String.sub s' 4)
                                                                 || String.sub s' 4 = #","
                                                                 || String.sub s' 4 = #"}") then
                                                              (r, String.suffix s' 4)
                                                          else
                                                              let
                                                                  val (v, s') = j.FromJson s'
                                                              in
                                                                  (r -- nm ++ {nm = Some v}, s')
                                                              end
                                                      else
                                                          let
                                                              val (r', s') = acc (r -- nm)
                                                          in
                                                              (r' ++ {nm = r.nm}, s')
                                                          end)
                                                  (fn r => (r, skipOne s'))
                                                  (@Folder.concat ! fl ofl)
                                                  (@map0 [fn _ => bool] (fn [t ::_] => False) fl
                                                    ++ @map0 [fn _ => bool] (fn [t ::_] => True) ofl)
                                                  (jss ++ ojss) (names ++ onames) r

                                   val s' = skipSpaces s'
                                   val s' = if s' <> "" && String.sub s' 0 = #"," then
                                                skipSpaces (String.suffix s' 1)
                                            else
                                                s'
                               in
                                   fromJ s' r
                               end
                   in
                       if s = "" || String.sub s 0 <> #"{" then
                           error <xml>JSON record doesn't begin with brace: {[firstTen s]}</xml>
                       else
                           let
                               val (r, s') = fromJ (skipSpaces (String.suffix s 1))
                                                   (@map0 [option] (fn [t ::_] => None) (@Folder.concat ! fl ofl))
                           in
                               (@map2 [option] [fn _ => string] [ident] (fn [t] (v : option t) name =>
                                                                            case v of
                                                                                None => error <xml>Missing JSON object field {[name]}</xml>
                                                                              | Some v => v) fl (r --- _) names
                                 ++ (r --- _), s')
                           end
                   end,
     ToYaml = fn i r =>
                 let
                     val withRequired =
                         @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                              "\n" ^ indent (i+1) ^ name ^ ": " ^ j.ToYaml (i+2) v ^ acc)
                          "" fl jss names (r --- _)

                     val withOptional =
                         @foldR3 [json] [fn _ => string] [option] [fn _ => string]
                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                              case v of
                                  None => acc
                                | Some v =>
                                  "\n" ^ indent (i+1) ^ name ^ ": " ^ j.ToYaml (i+2) v ^ acc)
                          withRequired ofl ojss onames (r --- _)
                 in
                     withOptional
                 end,
     FromYaml = fn b i s =>
                   let
                       fun fromY b s (r : $(map option (ts ++ ots))) : $(map option (ts ++ ots)) * string =
                           if s = "" then
                               (r, s)
                           else
                               let
                                   val (i', s') = readYamlLine (if b then Some i else None) s
                               in
                                   if i' < i then
                                       (r, s)
                                   else
                                       case String.split s' #":" of
                                           None =>
                                           if String.all Char.isSpace s' then
                                               (r, "")
                                           else
                                               error <xml>Bad label in YAML record</xml>
                                         | Some (name, s') =>
                                           let
                                               val s' = skipRealSpaces s'
                                               val (r, s') = @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> $(map option ts) * string]
                                                              (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                                                                  if name = name' then
                                                                      let
                                                                          val (v, s') = j.FromYaml False (i'+1) s'
                                                                      in
                                                                          (r -- nm ++ {nm = Some v}, s')
                                                                      end
                                                                  else
                                                                      let
                                                                          val (r', s') = acc (r -- nm)
                                                                      in
                                                                          (r' ++ {nm = r.nm}, s')
                                                                      end)
                                                              (fn r => (r, skipUntilIndentLowEnough i' s'))
                                                              (@Folder.concat ! fl ofl) (jss ++ ojss) (names ++ onames) r
                                           in
                                               fromY False s' r
                                           end
                               end

                       val r = @map0 [option] (fn [t ::_] => None) (@Folder.concat ! fl ofl)
                       val (r, s') =
                           if String.isPrefix {Full = s, Prefix = "{}"} then
                               (r, String.suffix s 2)
                           else
                               fromY b s r
                   in
                       (@map2 [option] [fn _ => string] [ident] (fn [t] (v : option t) name =>
                                                                    case v of
                                                                        None => error <xml>Missing YAML object field {[name]}</xml>
                                                                      | Some v => v) fl (r --- _) names
                         ++ (r --- _), s')
                   end}

(* At the moment, the below code is largely copied and pasted from the last
 * definition, because otherwise the compiler fails to inline enough for
 * compilation to succeed. *)
fun json_record [ts ::: {Type}] (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts)) : json $ts =
    {ToJson = fn r => "{" ^ @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                             (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                                 escape name ^ ": " ^ j.ToJson v ^ (case acc of
                                                                       "" => ""
                                                                     | acc => "," ^ acc))
                             "" fl jss names r ^ "}",
     FromJson = fn s =>
                   let
                       fun fromJ s (r : $(map option ts)) : $(map option ts) * string =
                           if s = "" then
                               error <xml>JSON object doesn't end in brace</xml>
                           else if String.sub s 0 = #"}" then
                               (r, String.suffix s 1)
                           else let
                                   val (name, s') = unescape s
                                   val s' = skipSpaces s'
                                   val s' = if s' = "" || String.sub s' 0 <> #":" then
                                                error <xml>No colon after JSON object field name</xml>
                                            else
                                                skipSpaces (String.suffix s' 1)

                                   val (r, s') = @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> $(map option ts) * string]
                                                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                                                      if name = name' then
                                                          let
                                                              val (v, s') = j.FromJson s'
                                                          in
                                                              (r -- nm ++ {nm = Some v}, s')
                                                          end
                                                      else
                                                          let
                                                              val (r', s') = acc (r -- nm)
                                                          in
                                                              (r' ++ {nm = r.nm}, s')
                                                          end)
                                                  (fn r => (r, skipOne s'))
                                                  fl jss names r

                                   val s' = skipSpaces s'
                                   val s' = if s' <> "" && String.sub s' 0 = #"," then
                                                skipSpaces (String.suffix s' 1)
                                            else
                                                s'
                               in
                                   fromJ s' r
                               end
                   in
                       if s = "" || String.sub s 0 <> #"{" then
                           error <xml>JSON record doesn't begin with brace: {[firstTen s]}</xml>
                       else
                           let
                               val (r, s') = fromJ (skipSpaces (String.suffix s 1))
                                                   (@map0 [option] (fn [t ::_] => None) fl)
                           in
                               (@map2 [option] [fn _ => string] [ident] (fn [t] (v : option t) name =>
                                                                            case v of
                                                                                None => error <xml>Missing JSON object field {[name]}</xml>
                                                                              | Some v => v) fl r names, s')
                           end
                   end,
     ToYaml = fn i r =>
                 @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                      "\n" ^ indent (i+1) ^ name ^ ": " ^ j.ToYaml (i+2) v ^ acc)
                  "" fl jss names (r --- _),
     FromYaml = fn b i s =>
                   let
                       fun fromY b s (r : $(map option ts)) : $(map option ts) * string =
                           if s = "" then
                               (r, s)
                           else
                               let
                                   val (i', s') = readYamlLine (if b then Some i else None) s
                               in
                                   if i' < i then
                                       (r, s)
                                   else
                                       case String.split s' #":" of
                                           None =>
                                           if String.all Char.isSpace s' then
                                               (r, "")
                                           else
                                               error <xml>Bad label in YAML record</xml>
                                         | Some (name, s') =>
                                           let
                                               val s' = skipRealSpaces s'
                                               val (r, s') = @foldR2 [json] [fn _ => string] [fn ts => $(map option ts) -> $(map option ts) * string]
                                                              (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name' acc r =>
                                                                  if name = name' then
                                                                      let
                                                                          val (v, s') = j.FromYaml False (i'+1) s'
                                                                      in
                                                                          (r -- nm ++ {nm = Some v}, s')
                                                                      end
                                                                  else
                                                                      let
                                                                          val (r', s') = acc (r -- nm)
                                                                      in
                                                                          (r' ++ {nm = r.nm}, s')
                                                                      end)
                                                              (fn r => (r, skipUntilIndentLowEnough i' s'))
                                                              fl jss names r
                                           in
                                               fromY False s' r
                                           end
                               end

                       val r = @map0 [option] (fn [t ::_] => None) fl
                       val (r, s') =
                           if String.isPrefix {Full = s, Prefix = "{}"} then
                               (r, String.suffix s 2)
                           else
                               fromY b s r
                   in
                       (@map2 [option] [fn _ => string] [ident] (fn [t] (v : option t) name =>
                                                                    case v of
                                                                        None => error <xml>Missing YAML object field {[name]}</xml>
                                                                      | Some v => v) fl r names, s')
                   end}

fun destrR [K] [f :: K -> Type] [fr :: K -> Type] [t ::: Type]
    (f : p :: K -> f p -> fr p -> t)
    [r ::: {K}] (fl : folder r) (v : variant (map f r)) (r : $(map fr r)) : t =
    match v
    (@Top.mp [fr] [fn p => f p -> t]
     (fn [p] (m : fr p) (v : f p) => f [p] v m)
     fl r)

fun json_variant [ts ::: {Type}] (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts)) : json (variant ts) =
    {ToJson = fn r => let val jnames = @map2 [json] [fn _ => string] [fn x => json x * string]
                                     (fn [t] (j : json t) (name : string) => (j, name)) fl jss names
                      in @destrR [ident] [fn x => json x * string]
                          (fn [p ::_] (v : p) (j : json p, name : string) =>
                            "{" ^ escape name ^ ": " ^ j.ToJson v ^ "}") fl r jnames
                      end,
     FromJson = fn s =>
                   if s = "" || String.sub s 0 <> #"{" then
                       error <xml>JSON variant doesn't begin with brace</xml>
                   else
                       let
                           val (name, s') = unescape (skipSpaces (String.suffix s 1))
                           val s' = skipSpaces s'
                           val s' = if s' = "" || String.sub s' 0 <> #":" then
                                        error <xml>No colon after JSON object field name</xml>
                                    else
                                        skipSpaces (String.suffix s' 1)

                           val (r, s') = (@foldR2 [json] [fn _ => string]
                                            [fn ts => ts' :: {Type} -> [ts ~ ts'] => variant (ts ++ ts') * string]
                                            (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest] (j : json t) name'
                                             (acc : ts' :: {Type} -> [rest ~ ts'] => variant (rest ++ ts') * string) [fwd ::_] [[nm = t] ++ rest ~ fwd] =>
                                                if name = name'
                                                    then
                                                        let val (v, s') = j.FromJson s'
                                                        in (make [nm] v, s')
                                                        end
                                                    else acc [fwd ++ [nm = t]]
                                            )
                                            (fn [fwd ::_] [[] ~ fwd] => error <xml>Unknown JSON object variant name {[name]}</xml>)
                                            fl jss names) [[]] !

                           val s' = skipSpaces s'
                           val s' = if s' <> "" && String.sub s' 0 = #"," then
                                        skipSpaces (String.suffix s' 1)
                                    else
                                        s'
                       in
                           if s' = "" then
                               error <xml>JSON object doesn't end in brace</xml>
                           else if String.sub s' 0 = #"}" then
                               (r, String.suffix s' 1)
                           else error <xml>Junk after JSON value in object</xml>
                       end,
     ToYaml = fn _ _ => error <xml>No YAML variants yet, please</xml>,
     FromYaml = fn _ _ _ => error <xml>No YAML variants yet, please</xml>}

val json_unit : json unit = json_record {} {}

fun foldl [a] [b] (f : a -> b -> b) =
    let
        fun foldl' acc ls =
            case ls of
                [] => acc
              | x :: ls => foldl' (f x acc) ls
    in
        foldl'
    end

val rev = fn [a] =>
             let
                 fun rev' acc (ls : list a) =
                     case ls of
                         [] => acc
                       | x :: ls => rev' (x :: acc) ls
             in
                 rev' []
             end

fun json_dict [a] (j : json a) : json (list (string * a)) =
    {ToJson = fn ls =>
                 foldl (fn (name, v) acc =>
                           let
                               val this = escape name ^ ":" ^ j.ToJson v
                           in
                               case acc of
                                   "{" => "{" ^ this
                                 | _ => acc ^ "," ^ this
                           end) "{" ls ^ "}",
     FromJson = fn s =>
                   let
                       fun fromJ (s : string) (acc : list (string * a)) : list (string * a) * string =
                           if s = "" then
                               error <xml>JSON object doesn't end in brace</xml>
                           else if String.sub s 0 = #"}" then
                               (rev acc, String.suffix s 1)
                           else let
                                   val (name, s') = unescape s
                                   val s' = skipSpaces s'
                                   val s' = if s' = "" || String.sub s' 0 <> #":" then
                                                error <xml>No colon after JSON object field name</xml>
                                            else
                                                skipSpaces (String.suffix s' 1)

                                   val (v, s') = j.FromJson s'

                                   val s' = skipSpaces s'
                                   val s' = if s' <> "" && String.sub s' 0 = #"," then
                                                skipSpaces (String.suffix s' 1)
                                            else
                                                s'
                               in
                                   fromJ s' ((name, v) :: acc)
                               end
                   in
                       if s = "" || String.sub s 0 <> #"{" then
                           error <xml>JSON dictionary doesn't begin with brace: {[firstTen s]}</xml>
                       else
                           fromJ (skipSpaces (String.suffix s 1)) []
                   end,
     ToYaml = fn i ls =>
                 foldl (fn (k, v) acc => indent i ^ k ^ ": " ^ j.ToYaml (i+1) v ^ acc) "" ls,
     FromYaml = fn b i s =>
                   let
                       fun fromY b s acc =
                           let
                               val (i', s') = readYamlLine (if b then Some i else None) s
                           in
                               if i' < i then
                                   (rev acc, s)
                               else
                                   case String.split s' #":" of
                                       None => error <xml>Couldn't find colon reading key-value list from YAML.</xml>
                                     | Some (name, s') =>
                                       let
                                           val (name', rest) = stringIn 0 name
                                           val (v, s') = j.FromYaml False (i'+1) (skipRealSpaces s')
                                       in
                                           if String.all Char.isSpace rest then
                                               fromY False s' ((name', v) :: acc)
                                           else
                                               error <xml>Malformed YAML key in dictionary: {[name]}</xml>
                                       end
                           end
                   in
                       if String.isPrefix {Full = s, Prefix = "{}"} then
                           ([], String.suffix s 2)
                       else
                           fromY b s []
                   end}

fun json_derived [base] [derived] (f1 : base -> derived) (f2 : derived -> base) (j : json base) =
    {ToJson = fn x => j.ToJson (f2 x),
     FromJson = fn s =>
                   let
                       val (x, s') = j.FromJson s
                   in
                       (f1 x, s')
                   end,
     ToYaml = fn i x => j.ToYaml i (f2 x),
     FromYaml = fn b i s =>
                   let
                       val (x, s') = j.FromYaml b i s
                   in
                       (f1 x, s')
                   end}

functor Recursive (M : sig
                       con t :: Type -> Type
                       val json_t : a ::: Type -> json a -> json (t a)
                   end) = struct
    open M

    datatype r = Rec of t r

    fun rTo (Rec x) = (json_t {ToJson = rTo,
                               FromJson = fn _ => error <xml>Tried to FromJson in ToJson!</xml>,
                               ToYaml = fn _ _ => error <xml>Tried to ToYaml in ToJson!</xml>,
                               FromYaml = fn _ _ _ => error <xml>Tried to FromYaml in ToJson!</xml>}).ToJson x

    fun rFrom s =
        let
            val (x, s') = (json_t {ToJson = fn _ => error <xml>Tried to ToJson in FromJson!</xml>,
                                   FromJson = rFrom,
                                   ToYaml = fn _ _ => error <xml>Tried to ToYaml in FromJson!</xml>,
                                   FromYaml = fn _ _ _ => error <xml>Tried to FromYaml in FromJson!</xml>}).FromJson s
        in
            (Rec x, s')
        end

    fun yTo i (Rec x) = (json_t {ToYaml = yTo,
                                 FromYaml = fn _ _ _ => error <xml>Tried to FromYaml in ToYaml!</xml>,
                                 ToJson = fn _ => error <xml>Tried to ToJson in ToYaml!</xml>,
                                 FromJson = fn _ => error <xml>Tried to FromJson in ToYaml!</xml>}).ToYaml i x

    fun yFrom b i s =
        let
            val (x, s') = (json_t {ToYaml = fn _ _ => error <xml>Tried to ToYaml in FromYaml!</xml>,
                                   FromYaml = yFrom,
                                   ToJson = fn _ => error <xml>Tried to ToJson in FromYaml!</xml>,
                                   FromJson = fn _ => error <xml>Tried to FromJson in FromYaml!</xml>}).FromYaml b i s
        in
            (Rec x, s')
        end

    val json_r = {ToJson = rTo, FromJson = rFrom, ToYaml = yTo, FromYaml = yFrom}
end

datatype prim = String of string | Int of int | Float of float | Bool of bool

fun primOut x =
    case x of
        String s => escape s
      | Int n => show n
      | Float n => show n
      | Bool True => "true"
      | Bool False => "false"

fun primIn s =
    if s = "" then
        error <xml>Reading primitive from empty JSON string</xml>
    else
        let
            val ch = String.sub s 0
        in
            if ch = #"\"" || ch = #"'" then
                let
                    val (r, s') = unescape s
                in
                    (String r, s')
                end
            else if String.isPrefix {Full = s, Prefix = "true"} then
                (Bool True, String.suffix s 4)
            else if String.isPrefix {Full = s, Prefix = "false"} then
                (Bool False, String.suffix s 5)
            else if Char.isDigit ch || ch = #"-" || ch = #"." then
                let
                    val (r, s') = numIn s
                in
                    case read r of
                        Some n => (Int n, s')
                      | None =>
                        case read r of
                            Some n => (Float n, s')
                          | None => error <xml>Invalid number in JSON</xml>
                end
            else
                error <xml>Didn't find primitive where expected in JSON</xml>
        end

val json_prim =
    {ToJson = primOut,
     ToYaml = fn _ => primOut,
     FromJson = primIn,
     FromYaml = fn _ _ => primIn}

val show_prim = mkShow (fn x =>
                           case x of
                               String s => s
                             | Int n => show n
                             | Float n => show n
                             | Bool b => show b)
