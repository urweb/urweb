(* Copyright (c) 2008-2011, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

structure Settings :> SETTINGS = struct

val urlPrefixFull = ref "/"
val urlPrefix = ref "/"
val urlPrePrefix = ref ""
val timeout = ref 0
val headers = ref ([] : string list)
val scripts = ref ([] : string list)

fun getUrlPrefixFull () = !urlPrefixFull
fun getUrlPrefix () = !urlPrefix
fun getUrlPrePrefix () = !urlPrePrefix
fun setUrlPrefix p =
    let
        val prefix = if p = "" then
                         "/"
                     else if String.sub (p, size p - 1) <> #"/" then
                         p ^ "/"
                     else
                         p

        fun findPrefix n =
            let
                val (befor, after) = Substring.splitl (fn ch => ch <> #"/") (Substring.extract (prefix, n, NONE))
            in
                if Substring.isEmpty after then
                    ("", prefix)
                else
                    (String.substring (prefix, 0, n) ^ Substring.string befor, Substring.string after)
            end

        val (prepre, prefix) =
            if String.isPrefix "http://" prefix then
                findPrefix 7
            else if String.isPrefix "https://" prefix then
                findPrefix 8
            else
                ("", prefix)
    in
        urlPrefixFull := p;
        urlPrePrefix := prepre;
        urlPrefix := prefix
    end

fun getTimeout () = !timeout
fun setTimeout n = timeout := n

fun getHeaders () = !headers
fun setHeaders ls = headers := ls

fun getScripts () = !scripts
fun setScripts ls = scripts := ls

type ffi = string * string

structure K = struct
type ord_key = ffi
fun compare ((m1, x1), (m2, x2)) =
    Order.join (String.compare (m1, m2),
             fn () => String.compare (x1, x2))
end

structure S = BinarySetFn(K)
structure M = BinaryMapFn(K)

fun basis x = S.addList (S.empty, map (fn x : string => ("Basis", x)) x)

val clientToServerBase = basis ["int",
                                "float",
                                "string",
                                "time",
                                "file",
                                "unit",
                                "option",
                                "list",
                                "bool",
                                "variant"]
val clientToServer = ref clientToServerBase
fun setClientToServer ls = clientToServer := S.addList (clientToServerBase, ls)
fun mayClientToServer x = S.member (!clientToServer, x)

val effectfulBase = basis ["dml",
                           "nextval",
                           "setval",
                           "set_cookie",
                           "clear_cookie",
                           "new_channel",
                           "send",
                           "htmlifyInt_w",
                           "htmlifyFloat_w",
                           "htmlifyString_w",
                           "htmlifyBool_w",
                           "htmlifyTime_w",
                           "attrifyInt_w",
                           "attrifyFloat_w",
                           "attrifyString_w",
                           "attrifyChar_w",
                           "urlifyInt_w",
                           "urlifyFloat_w",
                           "urlifyString_w",
                           "urlifyBool_w",
                           "urlifyChannel_w"]

val effectful = ref effectfulBase
fun setEffectful ls = effectful := S.addList (effectfulBase, ls)
fun isEffectful x = S.member (!effectful, x)

val benignBase = basis ["get_cookie",
                        "new_client_source",
                        "get_client_source",
                        "set_client_source",
                        "current",
                        "alert",
                        "confirm",
                        "onError",
                        "onFail",
                        "onConnectFail",
                        "onDisconnect",
                        "onServerError",
                        "kc",
                        "debug",
                        "rand",
                        "now",
                        "getHeader",
                        "setHeader",
                        "spawn",
                        "onClick",
                        "onDblclick",
                        "onKeydown",
                        "onKeypress",
                        "onKeyup",
                        "onMousedown",
                        "onMouseup",
                        "preventDefault",
                        "stopPropagation",
                        "fresh"]

val benign = ref benignBase
fun setBenignEffectful ls = benign := S.addList (benignBase, ls)
fun isBenignEffectful x = S.member (!benign, x)

val clientBase = basis ["get_client_source",
                        "current",
                        "alert",
                        "confirm",
                        "recv",
                        "sleep",
                        "spawn",
                        "onError",
                        "onFail",
                        "onConnectFail",
                        "onDisconnect",
                        "onServerError",
                        "kc",
                        "onClick",
                        "onDblclick",
                        "onKeydown",
                        "onKeypress",
                        "onKeyup",
                        "onMousedown",
                        "onMouseup",
                        "preventDefault",
                        "stopPropagation"]
val client = ref clientBase
fun setClientOnly ls = client := S.addList (clientBase, ls)
fun isClientOnly x = S.member (!client, x)

val serverBase = basis ["requestHeader",
                        "query",
                        "dml",
                        "nextval",
                        "setval",
                        "channel",
                        "send"]
val server = ref serverBase
fun setServerOnly ls = server := S.addList (serverBase, ls)
fun isServerOnly x = S.member (!server, x)

val basisM = foldl (fn ((k, v : string), m) => M.insert (m, ("Basis", k), v)) M.empty

val jsFuncsBase = basisM [("alert", "alert"),
                          ("stringToTime", "stringToTime"),
                          ("stringToTime_error", "stringToTime_error"),
                          ("timef", "strftime"),
                          ("confirm", "confrm"),
                          ("get_client_source", "sg"),
                          ("current", "scur"),
                          ("htmlifyBool", "bs"),
                          ("htmlifyFloat", "ts"),
                          ("htmlifyInt", "ts"),
                          ("htmlifyString", "eh"),
                          ("new_client_source", "sc"),
                          ("set_client_source", "sv"),
                          ("stringToFloat", "pflo"),
                          ("stringToInt", "pio"),
                          ("stringToFloat_error", "pfl"),
                          ("stringToInt_error", "pi"),
                          ("urlifyInt", "ts"),
                          ("urlifyFloat", "ts"),
                          ("urlifyTime", "ts"),
                          ("urlifyString", "uf"),
                          ("urlifyBool", "ub"),
                          ("recv", "rv"),
                          ("strcat", "cat"),
                          ("intToString", "ts"),
                          ("floatToString", "ts"),
                          ("charToString", "ts"),
                          ("onError", "onError"),
                          ("onFail", "onFail"),
                          ("onConnectFail", "onConnectFail"),
                          ("onDisconnect", "onDisconnect"),
                          ("onServerError", "onServerError"),
                          ("attrifyString", "atr"),
                          ("attrifyInt", "ts"),
                          ("attrifyFloat", "ts"),
                          ("attrifyBool", "bs"),
                          ("boolToString", "ts"),
                          ("str1", "id"),
                          ("strsub", "sub"),
                          ("strsuffix", "suf"),
                          ("strlen", "slen"),
                          ("strindex", "sidx"),
                          ("strchr", "schr"),
                          ("substring", "ssub"),
                          ("strcspn", "sspn"),
                          ("strlenGe", "strlenGe"),
                          ("kc", "kc"),
                          ("minTime", "0"),

                          ("islower", "isLower"),
                          ("isupper", "isUpper"),
                          ("isalpha", "isAlpha"),
                          ("isdigit", "isDigit"),
                          ("isalnum", "isAlnum"),
                          ("isblank", "isBlank"),
                          ("isspace", "isSpace"),
                          ("isxdigit", "isXdigit"),
                          ("tolower", "toLower"),
                          ("toupper", "toUpper"),

                          ("checkUrl", "checkUrl"),
                          ("bless", "bless"),

                          ("eq_time", "eq"),
                          ("lt_time", "lt"),
                          ("le_time", "le"),

                          ("debug", "uw_debug"),
                          ("naughtyDebug", "uw_debug"),

                          ("floatFromInt", "float"),
                          ("ceil", "ceil"),
                          ("trunc", "trunc"),
                          ("round", "round"),

                          ("now", "now"),
                          ("timeToString", "showTime"),
                          ("htmlifyTime", "showTimeHtml"),
                          ("toSeconds", "toSeconds"),
                          ("addSeconds", "addSeconds"),
                          ("diffInSeconds", "diffInSeconds"),

                          ("onClick", "uw_onClick"),
                          ("onDblclick", "uw_onDblclick"),
                          ("onKeydown", "uw_onKeydown"),
                          ("onKeypress", "uw_onKeypress"),
                          ("onKeyup", "uw_onKeyup"),
                          ("onMousedown", "uw_onMousedown"),
                          ("onMouseup", "uw_onMouseup"),
                          ("preventDefault", "uw_preventDefault"),
                          ("stopPropagation", "uw_stopPropagation"),

                          ("fresh", "fresh")]
val jsFuncs = ref jsFuncsBase
fun setJsFuncs ls = jsFuncs := foldl (fn ((k, v), m) => M.insert (m, k, v)) jsFuncsBase ls
fun jsFunc x = M.find (!jsFuncs, x)
fun allJsFuncs () = M.listItemsi (!jsFuncs)

datatype pattern_kind = Exact | Prefix
datatype action = Allow | Deny
type rule = { action : action, kind : pattern_kind, pattern : string }

datatype path_kind = Any | Url | Table | Sequence | View | Relation | Cookie | Style
type rewrite = { pkind : path_kind, kind : pattern_kind, from : string, to : string }

val rewrites = ref ([] : rewrite list)

fun subsume (pk1, pk2) =
    pk1 = pk2
    orelse pk2 = Any
    orelse pk2 = Relation andalso (pk1 = Table orelse pk1 = Sequence orelse pk1 = View)

fun setRewriteRules ls = rewrites := ls
fun rewrite pk s =
    let
        fun rew (ls : rewrite list) =
            case ls of
                [] => s
              | rewr :: ls =>
                let
                    fun match () =
                        case #kind rewr of
                            Exact => if #from rewr = s then
                                         SOME (size s)
                                     else
                                         NONE
                          | Prefix => if String.isPrefix (#from rewr) s then
                                          SOME (size (#from rewr))
                                      else
                                          NONE
                in
                    if subsume (pk, #pkind rewr) then
                        case match () of
                            NONE => rew ls
                          | SOME suffixStart => #to rewr ^ String.extract (s, suffixStart, NONE)
                    else
                        rew ls
                end
    in
        rew (!rewrites)
    end

val url = ref ([] : rule list)
val mime = ref ([] : rule list)
val request = ref ([] : rule list)
val response = ref ([] : rule list)

fun setUrlRules ls = url := ls
fun setMimeRules ls = mime := ls
fun setRequestHeaderRules ls = request := ls
fun setResponseHeaderRules ls = response := ls

fun getUrlRules () = !url
fun getMimeRules () = !mime
fun getRequestHeaderRules () = !request
fun getResponseHeaderRules () = !response

fun check f rules s =
    let
        fun chk (ls : rule list) =
            case ls of
                [] => false
              | rule :: ls =>
                let
                    val matches =
                        case #kind rule of
                            Exact => #pattern rule = s
                          | Prefix => String.isPrefix (#pattern rule) s
                in
                    if matches then
                        case #action rule of
                            Allow => true
                          | Deny => false
                    else
                        chk ls
                end
    in
        f s andalso chk (!rules)
    end

val checkUrl = check (fn _ => true) url

val validMime = CharVector.all (fn ch => Char.isAlphaNum ch orelse ch = #"/" orelse ch = #"-" orelse ch = #".")

val checkMime = check validMime mime
val checkRequestHeader = check validMime request
val checkResponseHeader = check validMime response


type protocol = {
     name : string,
     compile : string,
     linkStatic : string,
     linkDynamic : string,
     persistent : bool,
     code : unit -> Print.PD.pp_desc
}
val protocols = ref ([] : protocol list)
fun addProtocol p = protocols := p :: !protocols
fun getProtocol s = List.find (fn p => #name p = s) (!protocols)

fun clibFile s = OS.Path.joinDirFile {dir = Config.libC,
                                      file = s}

val curProto = ref {name = "",
                    compile = "",
                    linkStatic = "",
                    linkDynamic = "",
                    persistent = false,
                    code = fn () => Print.box []}
fun setProtocol name =
    case getProtocol name of
        NONE => raise Fail ("Unknown protocol " ^ name)
      | SOME p => curProto := p
fun currentProtocol () = !curProto

val debug = ref false
fun setDebug b = debug := b
fun getDebug () = !debug

datatype sql_type =
         Int
       | Float
       | String
       | Char
       | Bool
       | Time
       | Blob
       | Channel
       | Client
       | Nullable of sql_type

fun p_sql_ctype t =
    let
        open Print.PD
        open Print
    in
        case t of
            Int => "uw_Basis_int"
          | Float => "uw_Basis_float"
          | String => "uw_Basis_string"
          | Char => "uw_Basis_char"
          | Bool => "uw_Basis_bool"
          | Time => "uw_Basis_time"
          | Blob => "uw_Basis_blob"
          | Channel => "uw_Basis_channel"
          | Client => "uw_Basis_client"
          | Nullable String => "uw_Basis_string"
          | Nullable t => p_sql_ctype t ^ "*"
    end

fun isBlob Blob = true
  | isBlob (Nullable t) = isBlob t
  | isBlob _ = false

fun isNotNull (Nullable _) = false
  | isNotNull _ = true

datatype failure_mode = Error | None

type dbms = {
     name : string,
     header : string,
     link : string,
     p_sql_type : sql_type -> string,
     init : {dbstring : string,
             prepared : (string * int) list,
             tables : (string * (string * sql_type) list) list,
             views : (string * (string * sql_type) list) list,
             sequences : string list} -> Print.PD.pp_desc,
     query : {loc : ErrorMsg.span, cols : sql_type list,
              doCols : ({loc : ErrorMsg.span, wontLeakStrings : bool, col : int, typ : sql_type} -> Print.PD.pp_desc)
                       -> Print.PD.pp_desc}
             -> Print.PD.pp_desc,
     queryPrepared : {loc : ErrorMsg.span, id : int, query : string,
                      inputs : sql_type list, cols : sql_type list,
                      doCols : ({loc : ErrorMsg.span, wontLeakStrings : bool, col : int,
                                 typ : sql_type} -> Print.PD.pp_desc)
                               -> Print.PD.pp_desc,
                      nested : bool}
                     -> Print.PD.pp_desc,
     dml : ErrorMsg.span * failure_mode -> Print.PD.pp_desc,
     dmlPrepared : {loc : ErrorMsg.span, id : int, dml : string,
                    inputs : sql_type list, mode : failure_mode} -> Print.PD.pp_desc,
     nextval : {loc : ErrorMsg.span, seqName : string option, seqE : Print.PD.pp_desc} -> Print.PD.pp_desc,
     nextvalPrepared : {loc : ErrorMsg.span, id : int, query : string} -> Print.PD.pp_desc,
     setval : {loc : ErrorMsg.span, seqE : Print.PD.pp_desc, count : Print.PD.pp_desc} -> Print.PD.pp_desc,
     sqlifyString : string -> string,
     p_cast : string * sql_type -> string,
     p_blank : int * sql_type -> string,
     supportsDeleteAs : bool,
     supportsUpdateAs : bool,
     createSequence : string -> string,
     textKeysNeedLengths : bool,
     supportsNextval : bool,
     supportsNestedPrepared : bool,
     sqlPrefix : string,
     supportsOctetLength : bool,
     trueString : string,
     falseString : string,
     onlyUnion : bool,
     nestedRelops : bool
}

val dbmses = ref ([] : dbms list)
val curDb = ref ({name = "",
                  header = "",
                  link = "",
                  p_sql_type = fn _ => "",
                  init = fn _ => Print.box [],
                  query = fn _ => Print.box [],
                  queryPrepared = fn _ => Print.box [],
                  dml = fn _ => Print.box [],
                  dmlPrepared = fn _ => Print.box [],
                  nextval = fn _ => Print.box [],
                  nextvalPrepared = fn _ => Print.box [],
                  setval = fn _ => Print.box [],
                  sqlifyString = fn s => s,
                  p_cast = fn _ => "",
                  p_blank = fn _ => "",
                  supportsDeleteAs = false,
                  supportsUpdateAs = false,
                  createSequence = fn _ => "",
                  textKeysNeedLengths = false,
                  supportsNextval = false,
                  supportsNestedPrepared = false,
                  sqlPrefix = "",
                  supportsOctetLength = false,
                  trueString = "",
                  falseString = "",
                  onlyUnion = false,
                  nestedRelops = false} : dbms)

fun addDbms v = dbmses := v :: !dbmses
fun setDbms s =
    case List.find (fn db => #name db = s) (!dbmses) of
        NONE => raise Fail ("Unknown DBMS " ^ s)
      | SOME db => curDb := db
fun currentDbms () = !curDb

val dbstring = ref (NONE : string option)
fun setDbstring so = dbstring := so
fun getDbstring () = !dbstring

val exe = ref (NONE : string option)
fun setExe so = exe := so
fun getExe () = !exe

val sql = ref (NONE : string option)
fun setSql so = sql := so
fun getSql () = !sql

val coreInline = ref 20
fun setCoreInline n = coreInline := n
fun getCoreInline () = !coreInline

val monoInline = ref 100
fun setMonoInline n = monoInline := n
fun getMonoInline () = !monoInline

val staticLinking = ref false
fun setStaticLinking b = staticLinking := b
fun getStaticLinking () = !staticLinking

val deadlines = ref false
fun setDeadlines b = deadlines := b
fun getDeadlines () = !deadlines

val sigFile = ref (NONE : string option)
fun setSigFile v = sigFile := v
fun getSigFile () = !sigFile

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

val safeGet = ref SS.empty
fun setSafeGets ls = safeGet := SS.addList (SS.empty, ls)
fun isSafeGet x = SS.member (!safeGet, x)

val onError = ref (NONE : (string * string list * string) option)
fun setOnError x = onError := x
fun getOnError () = !onError

val limits = ["messages", "clients", "headers", "page", "heap", "script",
              "inputs", "subinputs", "cleanup", "deltas", "transactionals",
              "globals", "database", "time"]

val limitsList = ref ([] : (string * int) list)
fun addLimit (v as (name, _)) =
    if List.exists (fn name' => name' = name) limits then
        (limitsList := v :: !limitsList;
         if name = "time" then
             setDeadlines true
         else
             ())
    else
        raise Fail ("Unknown limit category '" ^ name ^ "'")
fun limits () = !limitsList

val minHeap = ref 0
fun setMinHeap n = if n >= 0 then minHeap := n else raise Fail "Trying to set negative minHeap"
fun getMinHeap () = !minHeap

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

val alwaysInline = ref SS.empty
fun addAlwaysInline s = alwaysInline := SS.add (!alwaysInline, s)
fun checkAlwaysInline s = SS.member (!alwaysInline, s)

val noXsrfProtection = ref SS.empty
fun addNoXsrfProtection s = noXsrfProtection := SS.add (!noXsrfProtection, s)
fun checkNoXsrfProtection s = SS.member (!noXsrfProtection, s)

val timeFormat = ref "%c"
fun setTimeFormat v = timeFormat := v
fun getTimeFormat () = !timeFormat

end
