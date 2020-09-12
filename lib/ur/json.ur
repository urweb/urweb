con json a = {ToJson : a -> string,
              FromJson : string -> a * string}

fun mkJson [a] (x : {ToJson : a -> string,
                     FromJson : string -> a * string}) = x

fun skipSpaces s =
    if s = "" then
        ""
    else if Char.isSpace (String.sub s 0) then
        skipSpaces (String.suffix s 1)
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
        fun findEnd i s =
            if s = "" then
                error <xml>JSON unescape: string ends before quote: {[s]}</xml>
            else
                let
                    val ch = String.sub s 0
                in
                    case ch of
                        #"\"" => i
                      | #"\\" =>
                        if not (strlenGe s 2) then
                            error <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                        else if String.sub s 1 = #"u" then
                            if not (strlenGe s 6) then
                                error <xml>JSON unescape: Bad escape sequence: {[s]}</xml>
                            else
                                findEnd (i+6) (String.suffix s 6)
                        else
                            findEnd (i+2) (String.suffix s 2)
                      | _ => findEnd (i+1) (String.suffix s 1)
                end

        val last = findEnd 1 (String.suffix s 1)

        fun unesc i s =
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
                                    ofUnicode n ^ unesc (i+6) (String.suffix s 6)
                                end
                        else
			    (case String.sub s 1 of
				 #"n" => "\n"
                               | #"r" => "\r"
                               | #"t" => "\t"
                               | #"\"" => "\""
			       | #"\\" => "\\"
			       | #"/" => "/"
                               | x => error <xml>JSON unescape: Bad escape char: {[x]}</xml>)
			    ^
			    unesc (i+2) (String.suffix s 2)
                      | _ => String.str ch ^ unesc (i+1) (String.suffix s 1)
                end
    in
        if s = "" || String.sub s 0 <> #"\"" then
            error <xml>JSON unescape: String doesn't start with double quote: {[s]}</xml>
        else
            (unesc 1 (String.suffix s 1), String.suffix s (last+1))
    end

val json_string = {ToJson = escape,
                   FromJson = unescape}

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

val json_time = {ToJson = fn tm => escape (rfc3339_out tm),
                 FromJson = fn s =>
                               let
                                   val (v, s') = unescape s
                               in
                                   (rfc3339_in v, s')
                               end}

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
                                                       FromJson = numIn}

val json_int = json_num
val json_float = json_num

val json_bool = {ToJson = fn b => if b then "true" else "false",
                 FromJson = fn s => if String.isPrefix {Full = s, Prefix = "true"} then
                                        (True, String.suffix s 4)
                                    else if String.isPrefix {Full = s, Prefix = "false"} then
                                        (False, String.suffix s 5)
                                    else
                                        error <xml>JSON: bad boolean string: {[s]}</xml>}

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
                            end}

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
    in
        {ToJson = toJ,
         FromJson = fromJ}
    end

fun skipOne s =
    let
        fun skipOne s dquote squote brace bracket =
            if s = "" then
                s
            else
                let
                    val ch = String.sub s 0
                    val rest = String.suffix s 1
                in
                    case ch of
                        #"\"" => skipOne rest (not dquote) squote brace bracket
                      | #"'" => skipOne rest dquote (not squote) brace bracket
                      | #"\\" => if rest <> "" then
                                     skipOne (String.suffix s 2) dquote squote brace bracket
                                 else
                                     ""
                      | #"{" => skipOne rest dquote squote (brace + 1) bracket
                      | #"}" => if brace = 0 then
                                    s
                                else
                                    skipOne rest dquote squote (brace - 1) bracket

                      | #"[" => skipOne rest dquote squote brace (bracket + 1)
                      | #"]" =>
                        if bracket = 0 then
                            s
                        else
                            skipOne rest dquote squote brace (bracket - 1)

                      | #"," =>
                        if not dquote && not squote && brace = 0 && bracket = 0 then
                            s
                        else
                            skipOne rest dquote squote brace bracket

                      | _ => skipOne rest dquote squote brace bracket
                end
    in
        skipOne s False False 0 0
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
                                                  (@Folder.concat ! fl ofl) (jss ++ ojss) (names ++ onames) r

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
                           error <xml>JSON record doesn't begin with brace</xml>
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
end}

(* At the moment, the below code is largely copied and pasted from the last
 * definition, because otherwise the compiler fails to inline enough for
 * compilation to succeed. *)
fun json_record [ts ::: {Type}] (fl : folder ts) (jss : $(map json ts)) (names : $(map (fn _ => string) ts)) : json $ts =
    {ToJson = fn r => "{" ^ @foldR3 [json] [fn _ => string] [ident] [fn _ => string]
                             (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (j : json t) name v acc =>
                                 escape name ^ ":" ^ j.ToJson v ^ (case acc of
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
                           error <xml>JSON record doesn't begin with brace</xml>
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
                            "{" ^ escape name ^ ":" ^ j.ToJson v ^ "}") fl r jnames
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
                       end
                   }

val json_unit : json unit = json_record {} {}

fun json_derived [base] [derived] (f1 : base -> derived) (f2 : derived -> base) (j : json base) =
    {ToJson = fn x => j.ToJson (f2 x),
     FromJson = fn s =>
                   let
                       val (x, s') = j.FromJson s
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
                               FromJson = fn _ => error <xml>Tried to FromJson in ToJson!</xml>}).ToJson x

    fun rFrom s =
        let
            val (x, s') = (json_t {ToJson = fn _ => error <xml>Tried to ToJson in FromJson!</xml>,
                                   FromJson = rFrom}).FromJson s
        in
            (Rec x, s')
        end

    val json_r = {ToJson = rTo, FromJson = rFrom}
end
