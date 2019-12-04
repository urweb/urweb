(* Copyright (c) 2008-2011, 2013, Adam Chlipala
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

signature SETTINGS = sig

    (* Call this when compiling a new project, e.g. with the Ur/Web daemon or from the SML/NJ REPL.
     * Some settings stay, but most are reset, especially files cached for the app to serve. *)
    val reset : unit -> unit

    (* XXX these should be unit -> string too *)
    val configBin : string ref
    val configLib : string ref
    val configSrcLib : string ref
    val configInclude : string ref
    val configSitelisp : string ref
    val configIcuIncludes : string ref
    val configIcuLibs : string ref

    val libUr : unit -> string
    val libC : unit -> string
    val libJs : unit -> string

    val setDebug : bool -> unit
    val getDebug : unit -> bool

    val libFile : string -> string
    val clibFile : string -> string

    (* How do all application URLs begin? *)
    val setUrlPrefix : string -> unit
    val getUrlPrefix : unit -> string
    val getUrlPrePrefix : unit -> string
    val getUrlPrefixFull : unit -> string
    (* The full prefix is the value that was set explicitly, while the "pre"
     * prefix gets the protocol/host/port part and the unqualified prefix gets
     * the URI. *)

    (* How many seconds should the server wait before assuming a Comet client has left? *)
    val setTimeout : int -> unit
    val getTimeout : unit -> int

    (* Which C header files are needed? *)
    val setHeaders : string list -> unit
    val getHeaders : unit -> string list

    (* Which extra JavaScript URLs should be included? *)
    val setScripts : string list -> unit
    val getScripts : unit -> string list

    type ffi = string * string

    (* Which FFI types may be sent from clients to servers? *)
    val setClientToServer : ffi list -> unit
    val mayClientToServer : ffi -> bool

    (* Which FFI functions have side effects? *)
    val setEffectful : ffi list -> unit
    val addEffectful : ffi -> unit
    val isEffectful : ffi -> bool

    (* Which FFI functions should not have their calls removed or reordered, but cause no lasting effects? *)
    val setBenignEffectful : ffi list -> unit
    val addBenignEffectful : ffi -> unit
    val isBenignEffectful : ffi -> bool

    (* Which FFI functions may only be run in clients? *)
    val setClientOnly : ffi list -> unit
    val addClientOnly : ffi -> unit
    val isClientOnly : ffi -> bool

    (* Which FFI functions may only be run on servers? *)
    val setServerOnly : ffi list -> unit
    val addServerOnly : ffi -> unit
    val isServerOnly : ffi -> bool

    (* Which FFI functions may be run in JavaScript?  (JavaScript function names included) *)
    val setJsModule : string option -> unit
    val setJsFuncs : (ffi * string) list -> unit
    val addJsFunc : ffi * string -> unit
    val jsFunc : ffi -> string option
    val allJsFuncs : unit -> (ffi * string) list

    datatype pattern_kind = Exact | Prefix
    datatype action = Allow | Deny
    type rule = { action : action, kind : pattern_kind, pattern : string }

    datatype path_kind = Any | Url | Table | Sequence | View | Relation | Cookie | Style
    type rewrite = { pkind : path_kind, kind : pattern_kind, from : string, to : string, hyphenate : bool }

    (* Rules for rewriting URLs from canonical forms *)
    val setRewriteRules : rewrite list -> unit
    val rewrite : path_kind -> string -> string

    (* Validating URLs and MIME types *)
    val setUrlRules : rule list -> unit
    val getUrlRules : unit -> rule list
    val checkUrl : string -> bool

    val setMimeRules : rule list -> unit
    val getMimeRules : unit -> rule list
    val checkMime : string -> bool

    val setRequestHeaderRules : rule list -> unit
    val getRequestHeaderRules : unit -> rule list
    val checkRequestHeader : string -> bool

    val setResponseHeaderRules : rule list -> unit
    val getResponseHeaderRules : unit -> rule list
    val checkResponseHeader : string -> bool

    val setEnvVarRules : rule list -> unit
    val getEnvVarRules : unit -> rule list
    val checkEnvVar : string -> bool

    val setMetaRules : rule list -> unit
    val getMetaRules : unit -> rule list
    val checkMeta : string -> bool

    (* Web protocols that generated programs may speak *)
    type protocol = {
        name : string,       (* Call it this on the command line *)
        compile : string,    (* Pass these `gcc -c' arguments *)
        linkStatic : string, (* Pass these static linker arguments *)
        linkDynamic : string,(* Pass these dynamic linker arguments *)
        persistent : bool,   (* Multiple requests per process? *)
        code : unit -> Print.PD.pp_desc (* Extra code to include in C files *)
    }
    val addProtocol : protocol -> unit
    val setProtocol : string -> unit
    val currentProtocol : unit -> protocol

    (* Different DBMSes *)
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

    val p_sql_ctype : sql_type -> string
    val isBlob : sql_type -> bool
    val isNotNull : sql_type -> bool

    datatype failure_mode = Error | None

    type dbms = {
         name : string,
         (* Call it this on the command line *)
         randomFunction : string,
         (* DBMS's name for random number-generating function *)
         header : string,
         (* Include this C header file *)
         link : string,
         (* Pass these linker arguments *)
         p_sql_type : sql_type -> string,
         init : {dbstring : string,
                 prepared : (string * int) list,
                 tables : (string * (string * sql_type) list) list,
                 views : (string * (string * sql_type) list) list,
                 sequences : string list} -> Print.PD.pp_desc,
         (* Define uw_client_init(), uw_db_init(), uw_db_close(), uw_db_begin(), uw_db_commit(), and uw_db_rollback() *)
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
         nextval : {loc : ErrorMsg.span, seqE : Print.PD.pp_desc, seqName : string option} -> Print.PD.pp_desc,
         nextvalPrepared : {loc : ErrorMsg.span, id : int, query : string} -> Print.PD.pp_desc,
         setval : {loc : ErrorMsg.span, seqE : Print.PD.pp_desc, count : Print.PD.pp_desc} -> Print.PD.pp_desc,
         sqlifyString : string -> string,
         p_cast : string * sql_type -> string,
         p_blank : int * sql_type -> string (* Prepared statement input *),
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
         nestedRelops : bool,
         windowFunctions : bool,
         requiresTimestampDefaults : bool,
         supportsIsDistinctFrom : bool,
         supportsSHA512 : {InitializeDb : string,
                           GenerateHash : string -> string} option,
         (* If supported, give the SQL code to
          * enable the feature in a particular
          * database and to compute a hash of a value. *)
         supportsSimilar : {InitializeDb : string} option
    }

    val addDbms : dbms -> unit
    val setDbms : string -> unit
    val currentDbms : unit -> dbms

    val setDbstring : string option -> unit
    val getDbstring : unit -> string option

    val setExe : string option -> unit
    val getExe : unit -> string option

    val setSql : string option -> unit
    val getSql : unit -> string option

    val setEndpoints : string option -> unit
    val getEndpoints : unit -> string option

    val setCoreInline : int -> unit
    val getCoreInline : unit -> int

    val setMonoInline : int -> unit
    val getMonoInline : unit -> int

    val setStaticLinking : bool -> unit
    val getStaticLinking : unit -> bool

    val setBootLinking : bool -> unit
    val getBootLinking : unit -> bool

    val setDeadlines : bool -> unit
    val getDeadlines : unit -> bool

    val setSigFile : string option -> unit
    val getSigFile : unit -> string option

    val setFileCache : string option -> unit
    val getFileCache : unit -> string option

    (* Which GET-able functions should be allowed to have side effects? *)
    val setSafeGetDefault : bool -> unit
    val setSafeGets : string list -> unit
    val isSafeGet : string -> bool

    val setOnError : (string * string list * string) option -> unit
    val getOnError : unit -> (string * string list * string) option

    val addLimit : string * int -> unit
    val limits : unit -> (string * int) list

    val setMinHeap : int -> unit
    val getMinHeap : unit -> int

    val addAlwaysInline : string -> unit
    val checkAlwaysInline : string -> bool

    val addNeverInline : string -> unit
    val checkNeverInline : string -> bool

    val addNoXsrfProtection : string -> unit
    val checkNoXsrfProtection : string -> bool

    val setTimeFormat : string -> unit
    val getTimeFormat : unit -> string

    val getCCompiler : unit -> string
    val setCCompiler : string -> unit

    val setMangleSql : bool -> unit
    val mangleSql : string -> string
    val mangleSqlCatalog : string -> string
    val mangleSqlTable : string -> string

    val setIsHtml5 : bool -> unit
    val getIsHtml5 : unit -> bool

    val setLessSafeFfi : bool -> unit
    val getLessSafeFfi : unit -> bool

    val setSqlcache : bool -> unit
    val getSqlcache : unit -> bool

    val setFilePath : string -> unit
    (* Sets the directory where we look for files being added below. *)

    val addFile : {Uri : string, LoadFromFilename : string, MimeType : string option} -> unit
    val listFiles : unit -> {Uri : string, ContentType : string option, LastModified : Time.time, Bytes : Word8Vector.vector} list

    val addJsFile : string (* filename *) -> unit
    val listJsFiles : unit -> {Filename : string, Content : string} list

    val setOutputJsFile : string option (* filename *) -> unit
    val getOutputJsFile : unit -> string option

    val setMimeFilePath : string -> unit
    (* Set unusual location for /etc/mime.types. *)
end
