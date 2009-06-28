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

structure Postgres :> POSTGRES = struct

open Settings
open Print.PD
open Print

fun init (dbstring, ss) =
    box [if #persistent (currentProtocol ()) then
             box [string "static void uw_db_prepare(uw_context ctx) {",
                  newline,
                  string "PGconn *conn = uw_get_db(ctx);",
                  newline,
                  string "PGresult *res;",
                  newline,
                  newline,

                  p_list_sepi newline (fn i => fn (s, n) =>
                                                  box [string "res = PQprepare(conn, \"uw",
                                                       string (Int.toString i),
                                                       string "\", \"",
                                                       string (String.toString s),
                                                       string "\", ",
                                                       string (Int.toString n),
                                                       string ", NULL);",
                                                       newline,
                                                       string "if (PQresultStatus(res) != PGRES_COMMAND_OK) {",
                                                       newline,
                                                       box [string "char msg[1024];",
                                                            newline,
                                                            string "strncpy(msg, PQerrorMessage(conn), 1024);",
                                                            newline,
                                                            string "msg[1023] = 0;",
                                                            newline,
                                                            string "PQclear(res);",
                                                            newline,
                                                            string "PQfinish(conn);",
                                                            newline,
                                                            string "uw_error(ctx, FATAL, \"Unable to create prepared statement:\\n",
                                                            string (String.toString s),
                                                            string "\\n%s\", msg);",
                                                            newline],
                                                       string "}",
                                                       newline,
                                                       string "PQclear(res);",
                                                       newline])
                              ss,

                  string "}",
                  newline,
                  newline,

                  string "void uw_db_close(uw_context ctx) {",
                  newline,
                  string "PQfinish(uw_get_db(ctx));",
                  newline,
                  string "}",
                  newline,
                  newline,

                  string "int uw_db_begin(uw_context ctx) {",
                  newline,
                  string "PGconn *conn = uw_get_db(ctx);",
                  newline,
                  string "PGresult *res = PQexec(conn, \"BEGIN ISOLATION LEVEL SERIALIZABLE\");",
                  newline,
                  newline,
                  string "if (res == NULL) return 1;",
                  newline,
                  newline,
                  string "if (PQresultStatus(res) != PGRES_COMMAND_OK) {",
                  box [string "PQclear(res);",
                       newline,
                       string "return 1;",
                       newline],
                  string "}",
                  newline,
                  string "return 0;",
                  newline,
                  string "}",
                  newline,
                  newline,

                  string "int uw_db_commit(uw_context ctx) {",
                  newline,
                  string "PGconn *conn = uw_get_db(ctx);",
                  newline,
                  string "PGresult *res = PQexec(conn, \"COMMIT\");",
                  newline,
                  newline,
                  string "if (res == NULL) return 1;",
                  newline,
                  newline,
                  string "if (PQresultStatus(res) != PGRES_COMMAND_OK) {",
                  box [string "PQclear(res);",
                       newline,
                       string "return 1;",
                       newline],
                  string "}",
                  newline,
                  string "return 0;",
                  newline,
                  string "}",
                  newline,
                  newline,

                  string "int uw_db_rollback(uw_context ctx) {",
                  newline,
                  string "PGconn *conn = uw_get_db(ctx);",
                  newline,
                  string "PGresult *res = PQexec(conn, \"ROLLBACK\");",
                  newline,
                  newline,
                  string "if (res == NULL) return 1;",
                  newline,
                  newline,
                  string "if (PQresultStatus(res) != PGRES_COMMAND_OK) {",
                  box [string "PQclear(res);",
                       newline,
                       string "return 1;",
                       newline],
                  string "}",
                  newline,
                  string "return 0;",
                  newline,
                  string "}",
                  newline,
                  newline]
         else
             string "static void uw_db_prepare(uw_context ctx) { }",
         newline,
         newline,

         string "void uw_db_init(uw_context ctx) {",
         newline,
         string "PGconn *conn = PQconnectdb(\"",
         string (String.toString dbstring),
         string "\");",
         newline,
         string "if (conn == NULL) uw_error(ctx, FATAL, ",
         string "\"libpq can't allocate a connection.\");",
         newline,
         string "if (PQstatus(conn) != CONNECTION_OK) {",
         newline,
         box [string "char msg[1024];",
              newline,
              string "strncpy(msg, PQerrorMessage(conn), 1024);",
              newline,
              string "msg[1023] = 0;",
              newline,
              string "PQfinish(conn);",
              newline,
              string "uw_error(ctx, BOUNDED_RETRY, ",
              string "\"Connection to Postgres server failed: %s\", msg);"],
         newline,
         string "}",
         newline,
         string "uw_set_db(ctx, conn);",
         newline,
         string "uw_db_validate(ctx);",
         newline,
         string "uw_db_prepare(ctx);",
         newline,
         string "}"]

val () = addDbms {name = "postgres",
                  header = "postgresql/libpq-fe.h",
                  link = "-lpq",
                  global_init = box [string "void uw_client_init() { }",
                                     newline],
                  init = init}
val () = setDbms "postgres"

end
