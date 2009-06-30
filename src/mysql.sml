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

structure MySQL :> MYSQL = struct

open Settings
open Print.PD
open Print

fun init {dbstring, prepared = ss, tables, sequences} =
    let
        val host = ref NONE
        val user = ref NONE
        val passwd = ref NONE
        val db = ref NONE
        val port = ref NONE
        val unix_socket = ref NONE

        fun stringOf r = case !r of
                             NONE => string "NULL"
                           | SOME s => box [string "\"",
                                            string (String.toString s),
                                            string "\""]
    in
        app (fn s =>
                case String.fields (fn ch => ch = #"=") s of
                    [name, value] =>
                    (case name of
                         "host" =>
                         if size value > 0 andalso String.sub (value, 0) = #"/" then
                             unix_socket := SOME value
                         else
                             host := SOME value
                       | "hostaddr" => host := SOME value
                       | "port" => port := Int.fromString value
                       | "dbname" => db := SOME value
                       | "user" => user := SOME value
                       | "password" => passwd := SOME value
                       | _ => ())
                  | _ => ()) (String.tokens Char.isSpace dbstring);

        box [string "typedef struct {",
             newline,
             box [string "MYSQL *conn;",
                  newline,
                  p_list_sepi (box [])
                              (fn i => fn _ =>
                                          box [string "MYSQL_STMT *p",
                                               string (Int.toString i),
                                               string ";",
                                               newline])
                              ss],
             string "} uw_conn;",
             newline,
             newline,

             if #persistent (currentProtocol ()) then
                 box [string "static void uw_db_prepare(uw_context ctx) {",
                      newline,
                      string "uw_conn *conn = uw_get_db(ctx);",
                      newline,
                      string "MYSQL_STMT *stmt;",
                      newline,
                      newline,

                      p_list_sepi newline (fn i => fn (s, n) =>
                                                      let
                                                          fun uhoh this s args =
                                                              box [p_list_sepi (box [])
                                                                               (fn j => fn () =>
                                                                                           box [string
                                                                                                    "mysql_stmt_close(conn->p",
                                                                                                string (Int.toString j),
                                                                                                string ");",
                                                                                                newline])
                                                                               (List.tabulate (i, fn _ => ())),
                                                                   box (if this then
                                                                            [string
                                                                                 "mysql_stmt_close(conn->p",
                                                                             string (Int.toString i),
                                                                             string ");",
                                                                             newline]
                                                                        else
                                                                            []),
                                                                   string "mysql_close(conn->conn);",
                                                                   newline,
                                                                   string "uw_error(ctx, FATAL, \"",
                                                                   string s,
                                                                   string "\"",
                                                                   p_list_sep (box []) (fn s => box [string ", ",
                                                                                                     string s]) args,
                                                                   string ");",
                                                                   newline]
                                                      in
                                                          box [string "stmt = mysql_stmt_init(conn->conn);",
                                                               newline,
                                                               string "if (stmt == NULL) {",
                                                               newline,
                                                               uhoh false "Out of memory allocating prepared statement" [],
                                                               string "}",
                                                               newline,

                                                               string "if (mysql_stmt_prepare(stmt, \"",
                                                               string (String.toString s),
                                                               string "\", ",
                                                               string (Int.toString (size s)),
                                                               string ")) {",
                                                               newline,
                                                               box [string "char msg[1024];",
                                                                    newline,
                                                                    string "strncpy(msg, mysql_stmt_error(stmt), 1024);",
                                                                    newline,
                                                                    string "msg[1023] = 0;",
                                                                    newline,
                                                                    uhoh true "Error preparing statement: %s" ["msg"]],
                                                               string "}",
                                                               newline]
                                                      end)
                                  ss,

                      string "}"]
             else
                 string "static void uw_db_prepare(uw_context ctx) { }",
             newline,
             newline,
             
             string "void uw_db_init(uw_context ctx) {",
             newline,
             string "MYSQL *mysql = mysql_init(NULL);",
             newline,
             string "uw_conn *conn;",
             newline,
             string "if (mysql == NULL) uw_error(ctx, FATAL, ",
             string "\"libmysqlclient can't allocate a connection.\");",
             newline,
             string "if (mysql_real_connect(mysql, ",
             stringOf host,
             string ", ",
             stringOf user,
             string ", ",
             stringOf passwd,
             string ", ",
             stringOf db,
             string ", ",
             case !port of
                 NONE => string "0"
               | SOME n => string (Int.toString n),
             string ", ",
             stringOf unix_socket,
             string ", 0)) {",
             newline,
             box [string "char msg[1024];",
                  newline,
                  string "strncpy(msg, mysql_error(mysql), 1024);",
                  newline,
                  string "msg[1023] = 0;",
                  newline,
                  string "mysql_close(mysql);",
                  newline,
                  string "uw_error(ctx, BOUNDED_RETRY, ",
                  string "\"Connection to MySQL server failed: %s\", msg);"],
             newline,
             string "}",
             newline,
             string "conn = calloc(1, sizeof(conn));",
             newline,
             string "conn->conn = mysql;",
             newline,
             string "uw_set_db(ctx, conn);",
             newline,
             string "uw_db_validate(ctx);",
             newline,
             string "uw_db_prepare(ctx);",
             newline,
             string "}",
             newline,
             newline,

             string "void uw_db_close(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             p_list_sepi (box [])
                         (fn i => fn _ =>
                                     box [string "if (conn->p",
                                          string (Int.toString i),
                                          string ") mysql_stmt_close(conn->p",
                                          string (Int.toString i),
                                          string ");",
                                          newline])
                         ss,
             string "mysql_close(conn->conn);",
             newline,
             string "}",
             newline,
             newline,

             string "int uw_db_begin(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             newline,
             string "return mysql_query(conn->conn, \"SET TRANSACTION ISOLATION LEVEL SERIALIZABLE\")",
             newline,
             string "  || mysql_query(conn->conn, \"BEGIN\");",
             newline,
             string "}",
             newline,
             newline,

             string "int uw_db_commit(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             string "return mysql_commit(conn->conn);",
             newline,
             string "}",
             newline,
             newline,

             string "int uw_db_rollback(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             string "return mysql_rollback(conn->conn);",
             newline,
             string "}",
             newline,
             newline]
    end

fun query _ = raise Fail "MySQL query"
fun queryPrepared _ = raise Fail "MySQL queryPrepared"
fun dml _ = raise Fail "MySQL dml"
fun dmlPrepared _ = raise Fail "MySQL dmlPrepared"
fun nextval _ = raise Fail "MySQL nextval"
fun nextvalPrepared _ = raise Fail "MySQL nextvalPrepared"

val () = addDbms {name = "mysql",
                  header = "mysql/mysql.h",
                  link = "-lmysqlclient",
                  global_init = box [string "void uw_client_init() {",
                                     newline,
                                     box [string "if (mysql_library_init(0, NULL, NULL)) {",
                                          newline,
                                          box [string "fprintf(stderr, \"Could not initialize MySQL library\\n\");",
                                               newline,
                                               string "exit(1);",
                                               newline],
                                          string "}",
                                          newline],
                              string "}",
                                     newline],
                  init = init,
                  query = query,
                  queryPrepared = queryPrepared,
                  dml = dml,
                  dmlPrepared = dmlPrepared,
                  nextval = nextval,
                  nextvalPrepared = nextvalPrepared}

end
