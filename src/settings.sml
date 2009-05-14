(* Copyright (c) 2008-2009, Adam Chlipala
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

val urlPrefix = ref "/"
val timeout = ref 0
val headers = ref ([] : string list)
val scripts = ref ([] : string list)

fun getUrlPrefix () = !urlPrefix
fun setUrlPrefix p =
    urlPrefix := (if p = "" then
                      "/"
                  else if String.sub (p, size p - 1) <> #"/" then
                      p ^ "/"
                  else
                      p)

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
                                "bool"]
val clientToServer = ref clientToServerBase
fun setClientToServer ls = clientToServer := S.addList (clientToServerBase, ls)
fun mayClientToServer x = S.member (!clientToServer, x)

val effectfulBase = basis ["dml",
                           "nextval",
                           "set_cookie",
                           "new_client_source",
                           "get_client_source",
                           "set_client_source",
                           "alert",
                           "new_channel",
                           "send",
                           "onError",
                           "onFail",
                           "onConnectFail",
                           "onDisconnect",
                           "onServerError"]

val effectful = ref effectfulBase
fun setEffectful ls = effectful := S.addList (effectfulBase, ls)
fun isEffectful x = S.member (!effectful, x)

val clientBase = basis ["get",
                        "set",
                        "alert",
                        "recv",
                        "sleep",
                        "spawn",
                        "onError",
                        "onFail",
                        "onConnectFail",
                        "onDisconnect",
                        "onServerError"]
val client = ref clientBase
fun setClientOnly ls = client := S.addList (clientBase, ls)
fun isClientOnly x = S.member (!client, x)

val serverBase = basis ["requestHeader",
                        "query",
                        "dml",
                        "nextval",
                        "channel",
                        "send"]
val server = ref serverBase
fun setServerOnly ls = server := S.addList (serverBase, ls)
fun isServerOnly x = S.member (!server, x)

val basisM = foldl (fn ((k, v : string), m) => M.insert (m, ("Basis", k), v)) M.empty

val jsFuncsBase = basisM [("alert", "alert"),
                          ("get_client_source", "sg"),
                          ("htmlifyBool", "bs"),
                          ("htmlifyFloat", "ts"),
                          ("htmlifyInt", "ts"),
                          ("htmlifyString", "eh"),
                          ("new_client_source", "sc"),
                          ("set_client_source", "sv"),
                          ("stringToFloat_error", "pfl"),
                          ("stringToInt_error", "pi"),
                          ("urlifyInt", "ts"),
                          ("urlifyFloat", "ts"),
                          ("urlifyString", "uf"),
                          ("recv", "rv"),
                          ("strcat", "cat"),
                          ("intToString", "ts"),
                          ("floatToString", "ts"),
                          ("onError", "onError"),
                          ("onFail", "onFail"),
                          ("onConnectFail", "onConnectFail"),
                          ("onDisconnect", "onDisconnect"),
                          ("onServerError", "onServerError"),
                          ("attrifyString", "escape"),
                          ("attrifyInt", "ts"),
                          ("attrifyFloat", "ts"),
                          ("attrifyBool", "bs")]
val jsFuncs = ref jsFuncsBase
fun setJsFuncs ls = jsFuncs := foldl (fn ((k, v), m) => M.insert (m, k, v)) jsFuncsBase ls
fun jsFunc x = M.find (!jsFuncs, x)

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

fun setUrlRules ls = url := ls
fun setMimeRules ls = mime := ls

fun getUrlRules () = !url
fun getMimeRules () = !mime

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
val checkMime = check
                    (CharVector.all (fn ch => Char.isAlphaNum ch orelse ch = #"/" orelse ch = #"-" orelse ch = #"."))
                    mime

end
