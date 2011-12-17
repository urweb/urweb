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

signature SETTINGS = sig
    
    val setDebug : bool -> unit
    val getDebug : unit -> bool
                           
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
    val isEffectful : ffi -> bool

    (* Which FFI functions should not have their calls removed or reordered, but cause no lasting effects? *)
    val setBenignEffectful : ffi list -> unit
    val isBenignEffectful : ffi -> bool

    (* Which FFI functions may only be run in clients? *)
    val setClientOnly : ffi list -> unit
    val isClientOnly : ffi -> bool

    (* Which FFI functions may only be run on servers? *)
    val setServerOnly : ffi list -> unit
    val isServerOnly : ffi -> bool

    (* Which FFI functions may be run in JavaScript?  (JavaScript function names included) *)
    val setJsFuncs : (ffi * string) list -> unit
    val jsFunc : ffi -> string option
    val allJsFuncs : unit -> (ffi * string) list

    datatype pattern_kind = Exact | Prefix
    datatype action = Allow | Deny
    type rule = { action : action, kind : pattern_kind, pattern : string }

    datatype path_kind = Any | Url | Table | Sequence | View | Relation | Cookie | Style
    type rewrite = { pkind : path_kind, kind : pattern_kind, from : string, to : string }

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
         nestedRelops : bool
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

    val setCoreInline : int -> unit
    val getCoreInline : unit -> int

    val setMonoInline : int -> unit
    val getMonoInline : unit -> int

    val setStaticLinking : bool -> unit
    val getStaticLinking : unit -> bool

    val setDeadlines : bool -> unit
    val getDeadlines : unit -> bool

    val setSigFile : string option -> unit
    val getSigFile : unit -> string option

    (* Which GET-able functions should be allowed to have side effects? *)
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

    val addNoXsrfProtection : string -> unit
    val checkNoXsrfProtection : string -> bool

    val setTimeFormat : string -> unit
    val getTimeFormat : unit -> string
end
