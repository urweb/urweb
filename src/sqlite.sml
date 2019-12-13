 (* Copyright (c) 2009-2010, Adam Chlipala
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

structure SQLite :> SQLITE = struct

open Settings
open Print.PD
open Print

fun p_sql_type t =
    case t of
        Int => "integer"
      | Float => "real"
      | String => "text"
      | Char => "text"
      | Bool => "integer"
      | Time => "text"
      | Blob => "blob"
      | Channel => "integer"
      | Client => "integer"
      | Nullable t => p_sql_type t

val ident = String.translate (fn #"'" => "PRIME"
                               | ch => str ch)

fun checkRel (table, checkNullable) (s, xts) =
    let
        val q = "SELECT COUNT(*) FROM sqlite_master WHERE type = '" ^ table ^ "' AND name = '"
                ^ s ^ "'"
    in
        box [string "if (sqlite3_prepare_v2(conn->conn, \"",
             string q,
             string "\", -1, &stmt, NULL) != SQLITE_OK) {",
             newline,
             box [string "sqlite3_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Query preparation failed:<br />",
                  string q,
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "while ((res = sqlite3_step(stmt)) == SQLITE_BUSY)",
             newline,
             box [string "sleep(1);",
                  newline],
             newline,
             string "if (res == SQLITE_DONE) {",
             newline,
             box [string "sqlite3_finalize(stmt);",
                  newline,
                  string "sqlite3_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"No row returned:<br />",
                  string q,
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,
             string "if (res != SQLITE_ROW) {",
             newline,
             box [string "sqlite3_finalize(stmt);",
                  newline,
                  string "sqlite3_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Error getting row:<br />",
                  string q,
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if (sqlite3_column_count(stmt) != 1) {",
             newline,
             box [string "sqlite3_finalize(stmt);",
                  newline,
                  string "sqlite3_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Bad column count:<br />",
                  string q,
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if (sqlite3_column_int(stmt, 0) != 1) {",
             newline,
             box [string "sqlite3_finalize(stmt);",
                  newline,
                  string "sqlite3_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Table '",
                  string s,
                  string "' does not exist.\");",
                  newline],
             string "}",
             newline,
             newline,
             string "sqlite3_finalize(stmt);",
             newline]
    end

fun init {dbstring, prepared = ss, tables, views, sequences} =
    let
        val db = ref dbstring
    in
        app (fn s =>
                case String.fields (fn ch => ch = #"=") s of
                    [name, value] =>
                    (case name of
                         "dbname" => db := value
                       | _ => ())
                  | _ => ()) (String.tokens Char.isSpace dbstring);

        box [string "typedef struct {",
             newline,
             box [string "sqlite3 *conn;",
                  newline,
                  p_list_sepi (box [])
                              (fn i => fn _ =>
                                          box [string "sqlite3_stmt *p",
                                               string (Int.toString i),
                                               string ";",
                                               newline])
                              ss],
             string "} uw_conn;",
             newline,
             newline,

             string "static void uw_client_init(void) {",
             newline,
             box [string "uw_sqlfmtInt = \"%lld%n\";",
                  newline,
                  string "uw_sqlfmtFloat = \"%.16g%n\";",
                  newline,
                  string "uw_Estrings = 0;",
                  newline,
                  string "uw_sql_type_annotations = 0;",
                  newline,
                  string "uw_sqlsuffixString = \"\";",
                  newline,
                  string "uw_sqlsuffixChar = \"\";",
                  newline,
                  string "uw_sqlsuffixBlob = \"\";",
                  newline,
                  string "uw_sqlfmtUint4 = \"%u%n\";",
                  newline],
             string "}",
             newline,
             newline,

             if #persistent (currentProtocol ()) then
                 box [string "static void uw_db_validate(uw_context ctx) {",
                      newline,
                      string "uw_conn *conn = uw_get_db(ctx);",
                      newline,
                      string "sqlite3_stmt *stmt;",
                      newline,
                      string "int res;",
                      newline,
                      newline,
                      p_list_sep newline (checkRel ("table", true)) tables,
                      p_list_sep newline (fn name => checkRel ("table", true)
                                                              (name, [("id", Settings.Client)])) sequences,
                      p_list_sep newline (checkRel ("view", false)) views,
                      string "}",
                      newline,
                      newline,

                      string "static void uw_db_prepare(uw_context ctx) {",
                      newline,
                      string "uw_conn *conn = uw_get_db(ctx);",
                      newline,
                      newline,

                      p_list_sepi newline (fn i => fn (s, _) =>
                                                      let
                                                          fun uhoh this s args =
                                                              box [p_list_sepi (box [])
                                                                               (fn j => fn () =>
                                                                                           box [string
                                                                                                    "sqlite3_finalize(conn->p",
                                                                                                string (Int.toString j),
                                                                                                string ");",
                                                                                                newline])
                                                                               (List.tabulate (i, fn _ => ())),
                                                                   box (if this then
                                                                            [string
                                                                                 "sqlite3_finalize(conn->p",
                                                                             string (Int.toString i),
                                                                             string ");",
                                                                             newline]
                                                                        else
                                                                            []),
                                                                   string "sqlite3_close(conn->conn);",
                                                                   newline,
                                                                   string "uw_error(ctx, FATAL, \"",
                                                                   string s,
                                                                   string "\"",
                                                                   p_list_sep (box []) (fn s => box [string ", ",
                                                                                                     string s]) args,
                                                                   string ");",
                                                                   newline]
                                                      in
                                                          box [string "if (sqlite3_prepare_v2(conn->conn, \"",
                                                               string (Prim.toCString s),
                                                               string "\", -1, &conn->p",
                                                               string (Int.toString i),
                                                               string ", NULL) != SQLITE_OK) {",
                                                               newline,
                                                               box [string "char msg[1024];",
                                                                    newline,
                                                                    string "strncpy(msg, sqlite3_errmsg(conn->conn), 1024);",
                                                                    newline,
                                                                    string "msg[1023] = 0;",
                                                                    newline,
                                                                    uhoh false ("Error preparing statement: "
                                                                                ^ Prim.toCString s ^ "<br />%s") ["msg"]],
                                                               string "}",
                                                               newline]
                                                      end)
                                  ss,

                      string "}"]
             else
                 box [string "static void uw_db_prepare(uw_context ctx) { }",
                      newline,
                      string "static void uw_db_validate(uw_context ctx) { }"],
             newline,
             newline,

             string "static void uw_db_init(uw_context ctx) {",
             newline,
             string "sqlite3 *sqlite;",
             newline,
             string "sqlite3_stmt *stmt;",
             newline,
             string "uw_conn *conn;",
             newline,
             newline,
             string "if (sqlite3_open(\"",
             string (!db),
             string "\", &sqlite) != SQLITE_OK) uw_error(ctx, FATAL, ",
             string "\"Can't open SQLite database.\");",
             newline,
             newline,
             string "if (sqlite3_exec(sqlite, \"PRAGMA foreign_keys = ON\", NULL, NULL, NULL) != SQLITE_OK)",
             newline,
             box [string "uw_error(ctx, FATAL, \"Can't enable foreign_keys for SQLite database\");",
                  newline],
             newline,
             string "if (uw_database_max < SIZE_MAX) {",
             newline,
             box [string "char buf[100];",
                  newline,
                  newline,

                  string "sprintf(buf, \"PRAGMA max_page_count = %llu\", (unsigned long long)(uw_database_max / 1024));",
                  newline,
                  newline,

                  string "if (sqlite3_prepare_v2(sqlite, buf, -1, &stmt, NULL) != SQLITE_OK) {",
                  newline,
                  box [string "sqlite3_close(sqlite);",
                       newline,
                       string "uw_error(ctx, FATAL, \"Can't prepare max_page_count query for SQLite database\");",
                       newline],
                  string "}",
                  newline,
                  newline,

                  string "if (sqlite3_step(stmt) != SQLITE_ROW) {",
                  newline,
                  box [string "sqlite3_finalize(stmt);",
                       newline,
                       string "sqlite3_close(sqlite);",
                       newline,
                       string "uw_error(ctx, FATAL, \"Can't set max_page_count parameter for SQLite database\");",
                       newline],
                  string "}",
                  newline,
                  newline,

                  string "sqlite3_finalize(stmt);",
                  newline],
             string "}",
             newline,
             newline,

             string "conn = calloc(1, sizeof(uw_conn));",
             newline,
             string "conn->conn = sqlite;",
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

             string "static void uw_db_close(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             p_list_sepi (box [])
                         (fn i => fn _ =>
                                     box [string "if (conn->p",
                                          string (Int.toString i),
                                          string ") sqlite3_finalize(conn->p",
                                          string (Int.toString i),
                                          string ");",
                                          newline])
                         ss,
             string "sqlite3_close(conn->conn);",
             newline,
             string "}",
             newline,
             newline,

             string "static int uw_db_begin(uw_context ctx, int could_write) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             newline,
             string "if (sqlite3_exec(conn->conn, \"BEGIN\", NULL, NULL, NULL) == SQLITE_OK)",
             newline,
             box [string "return 0;",
                  newline],
             string "else {",
             newline,
             box [string "fprintf(stderr, \"Begin error: %s<br />\", sqlite3_errmsg(conn->conn));",
                  newline,
                  string "return 1;",
                  newline],
             string "}",
             newline,
             string "}",
             newline,
             string "static int uw_db_commit(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             string "if (sqlite3_exec(conn->conn, \"COMMIT\", NULL, NULL, NULL) == SQLITE_OK)",
             newline,
             box [string "return 0;",
                  newline],
             string "else {",
             newline,
             box [string "fprintf(stderr, \"Commit error: %s<br />\", sqlite3_errmsg(conn->conn));",
                  newline,
                  string "return 1;",
                  newline],
             string "}",
             newline,
             string "}",
             newline,
             newline,

             string "static int uw_db_rollback(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             string "if (sqlite3_exec(conn->conn, \"ROLLBACK\", NULL, NULL, NULL) == SQLITE_OK)",
             newline,
             box [string "return 0;",
                  newline],
             string "else {",
             newline,
             box [string "fprintf(stderr, \"Rollback error: %s<br />\", sqlite3_errmsg(conn->conn));",
                  newline,
                  string "return 1;",
                  newline],
             string "}",
             newline,
             string "}",
             newline,
             newline]
    end

val fmt = "\"%Y-%m-%d %H:%M:%S\""

fun p_getcol {loc, wontLeakStrings, col = i, typ = t} =
    let
        fun p_unsql t =
            case t of
                Int => box [string "sqlite3_column_int64(stmt, ", string (Int.toString i), string ")"]
              | Float => box [string "sqlite3_column_double(stmt, ", string (Int.toString i), string ")"]
              | String =>
                if wontLeakStrings then
                    box [string "(uw_Basis_string)sqlite3_column_text(stmt, ", string (Int.toString i), string ")"]
                else
                    box [string "uw_strdup(ctx, (uw_Basis_string)sqlite3_column_text(stmt, ", string (Int.toString i), string "))"]
              | Char => box [string "sqlite3_column_text(stmt, ", string (Int.toString i), string ")[0]"]
              | Bool => box [string "(uw_Basis_bool)sqlite3_column_int(stmt, ", string (Int.toString i), string ")"]
              | Time => box [string "uw_Basis_stringToTimef_error(ctx, ",
                             string fmt,
                             string ", (uw_Basis_string)sqlite3_column_text(stmt, ",
                             string (Int.toString i),
                             string "))"]
              | Blob => box [string "({",
                             newline,
                             string "char *data = (char *)sqlite3_column_blob(stmt, ",
                             string (Int.toString i),
                             string ");",
                             newline,
                             string "int len = sqlite3_column_bytes(stmt, ",
                             string (Int.toString i),
                             string ");",
                             newline,
                             string "uw_Basis_blob b = {len, uw_memdup(ctx, data, len)};",
                             newline,
                             string "b;",
                             newline,
                             string "})"]
              | Channel => box [string "({",
                                newline,
                                string "sqlite3_int64 n = sqlite3_column_int64(stmt, ",
                                string (Int.toString i),
                                string ");",
                                newline,
                                string "uw_Basis_channel ch = {n >> 32, n & 0xFFFFFFFF};",
                                newline,
                                string "ch;",
                                newline,
                                string "})"]
              | Client => box [string "sqlite3_column_int(stmt, ", string (Int.toString i), string ")"]

              | Nullable _ => raise Fail "Postgres: Recursive Nullable"

        fun getter t =
            case t of
                Nullable t =>
                box [string "(sqlite3_column_type(stmt, ",
                     string (Int.toString i),
                     string ") == SQLITE_NULL ? NULL : ",
                     case t of
                         String => getter t
                       | _ => box [string "({",
                                   newline,
                                   string (p_sql_ctype t),
                                   space,
                                   string "*tmp = uw_malloc(ctx, sizeof(",
                                   string (p_sql_ctype t),
                                   string "));",
                                   newline,
                                   string "*tmp = ",
                                   getter t,
                                   string ";",
                                   newline,
                                   string "tmp;",
                                   newline,
                                   string "})"],
                     string ")"]
              | _ =>
                box [string "(sqlite3_column_type(stmt, ",
                     string (Int.toString i),
                     string ") == SQLITE_NULL ? ",
                     box [string "({",
                          string (p_sql_ctype t),
                          space,
                          string "tmp;",
                          newline,
                          string "uw_error(ctx, FATAL, \"",
                          string (ErrorMsg.spanToString loc),
                          string ": Unexpectedly NULL field #",
                          string (Int.toString i),
                          string "\");",
                          newline,
                          string "tmp;",
                          newline,
                          string "})"],
                     string " : ",
                     p_unsql t,
                     string ")"]
    in
        getter t
    end

fun queryCommon {loc, query, cols, doCols} =
    box [string "int r;",
         newline,

         string "sqlite3_reset(stmt);",
         newline,

         string "uw_end_region(ctx);",
         newline,
         string "while ((r = sqlite3_step(stmt)) == SQLITE_ROW) {",
         newline,
         doCols p_getcol,
         string "}",
         newline,
         newline,

         string "if (r == SQLITE_BUSY) {",
         box [string "sleep(1);",
              newline,
              string "uw_error(ctx, UNLIMITED_RETRY, \"Database is busy\");",
              newline],
         string "}",
         newline,
         newline,

         string "if (r != SQLITE_DONE) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": query step failed: %s<br />%s\", ",
         query,
         string ", sqlite3_errmsg(conn->conn));",
         newline,
         newline]

fun query {loc, cols, doCols} =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         string "sqlite3_stmt *stmt;",
         newline,
         newline,
         string "if (sqlite3_prepare_v2(conn->conn, query, -1, &stmt, NULL) != SQLITE_OK) uw_error(ctx, FATAL, \"Error preparing statement: %s<br />%s\", sqlite3_errmsg(conn->conn), query);",
         newline,
         newline,
         string "uw_push_cleanup(ctx, (void (*)(void *))sqlite3_finalize, stmt);",
         newline,
         newline,

         queryCommon {loc = loc, cols = cols, doCols = doCols, query = string "query"},

         string "uw_pop_cleanup(ctx);",
         newline]

val p_pre_inputs =
    p_list_sepi (box [])
                (fn i => fn t =>
                            case t of
                                Char => box [string "char arg",
                                             string (Int.toString (i + 1)),
                                             string "s = {arg",
                                             string (Int.toString (i + 1)),
                                             string ", 0};",
                                             newline]
                              | _ => box [])

fun p_inputs loc =
    p_list_sepi (box [])
                (fn i => fn t =>
                            let
                                fun bind (t, arg) =
                                    case t of
                                        Int => box [string "sqlite3_bind_int64(stmt, ",
                                                    string (Int.toString (i + 1)),
                                                    string ", ",
                                                    arg,
                                                    string ")"]
                                      | Float => box [string "sqlite3_bind_double(stmt, ",
                                                      string (Int.toString (i + 1)),
                                                      string ", ",
                                                      arg,
                                                      string ")"]
                                      | String => box [string "sqlite3_bind_text(stmt, ",
                                                       string (Int.toString (i + 1)),
                                                       string ", ",
                                                       arg,
                                                       string ", -1, SQLITE_TRANSIENT)"]
                                      | Char => box [string "sqlite3_bind_text(stmt, ",
                                                     string (Int.toString (i + 1)),
                                                     string ", ",
                                                     arg,
                                                     string "s, -1, SQLITE_TRANSIENT)"]
                                      | Bool => box [string "sqlite3_bind_int(stmt, ",
                                                     string (Int.toString (i + 1)),
                                                     string ", ",
                                                     arg,
                                                     string ")"]
                                      | Time => box [string "sqlite3_bind_text(stmt, ",
                                                     string (Int.toString (i + 1)),
                                                     string ", uw_Basis_timef(ctx, ",
                                                     string fmt,
                                                     string ", ",
                                                     arg,
                                                     string "), -1, SQLITE_TRANSIENT)"]
                                      | Blob => box [string "sqlite3_bind_blob(stmt, ",
                                                     string (Int.toString (i + 1)),
                                                     string ", ",
                                                     arg,
                                                     string ".data, ",
                                                     arg,
                                                     string ".size, SQLITE_TRANSIENT)"]
                                      | Channel => box [string "sqlite3_bind_int64(stmt, ",
                                                        string (Int.toString (i + 1)),
                                                        string ", ((sqlite3_int64)",
                                                        arg,
                                                        string ".cli << 32) | ",
                                                        arg,
                                                        string ".chn)"]
                                      | Client => box [string "sqlite3_bind_int(stmt, ",
                                                       string (Int.toString (i + 1)),
                                                       string ", ",
                                                       arg,
                                                       string ")"]
                                      | Nullable t => box [string "(",
                                                           arg,
                                                           string " == NULL ? sqlite3_bind_null(stmt, ",
                                                           string (Int.toString (i + 1)),
                                                           string ") : ",
                                                           bind (t, case t of
                                                                        String => arg
                                                                      | _ => box [string "(*", arg, string ")"]),
                                                           string ")"]
                            in
                                box [string "if (",
                                     bind (t, box [string "arg", string (Int.toString (i + 1))]),
                                     string " != SQLITE_OK) uw_error(ctx, FATAL, \"",
                                     string (ErrorMsg.spanToString loc),
                                     string ": Error binding parameter #",
                                     string (Int.toString (i + 1)),
                                     string ": %s\", sqlite3_errmsg(conn->conn));",
                                     newline]
                            end)

fun queryPrepared {loc, id, query, inputs, cols, doCols, nested} =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         p_pre_inputs inputs,
         if nested then
             box [string "sqlite3_stmt *stmt;",
                  newline]
         else
             box [string "sqlite3_stmt *stmt = conn->p",
                  string (Int.toString id),
                  string ";",
                  newline,
                  newline,

                  string "if (stmt == NULL) {",
                  newline],

         string "if (sqlite3_prepare_v2(conn->conn, \"",
         string (Prim.toCString query),
         string "\", -1, &stmt, NULL) != SQLITE_OK) uw_error(ctx, FATAL, \"Error preparing statement: ",
         string (Prim.toCString query),
         string "<br />%s\", sqlite3_errmsg(conn->conn));",
         newline,
         if nested then
             box [string "uw_push_cleanup(ctx, (void (*)(void *))sqlite3_finalize, stmt);",
                  newline]
         else
             box [string "conn->p",
                  string (Int.toString id),
                  string " = stmt;",
                  newline,
                  string "}",
                  newline,
                  newline,
                  string "uw_push_cleanup(ctx, (void (*)(void *))sqlite3_clear_bindings, stmt);",
                  newline,
                  string "uw_push_cleanup(ctx, (void (*)(void *))sqlite3_reset, stmt);",
                  newline],
         newline,

         p_inputs loc inputs,
         newline,

         queryCommon {loc = loc, cols = cols, doCols = doCols, query = box [string "\"",
                                                                            string (Prim.toCString query),
                                                                            string "\""]},

         string "uw_pop_cleanup(ctx);",
         newline,
         if nested then
             box []
         else
             box [string "uw_pop_cleanup(ctx);",
                  newline]]

fun dmlCommon {loc, dml, mode} =
    box [string "int r;",
         newline,

         string "if ((r = sqlite3_step(stmt)) == SQLITE_BUSY) {",
         box [string "sleep(1);",
              newline,
              string "uw_error(ctx, UNLIMITED_RETRY, \"Database is busy\");",
              newline],
         string "}",
         newline,
         newline,

         string "if (r != SQLITE_DONE) ",
         case mode of
             Settings.Error => box [string "uw_error(ctx, FATAL, \"",
                                    string (ErrorMsg.spanToString loc),
                                    string ": DML step failed: %s<br />%s\", ",
                                    dml,
                                    string ", sqlite3_errmsg(conn->conn));"]
           | Settings.None => string "uw_set_error_message(ctx, sqlite3_errmsg(conn->conn));",
         newline]

fun dml (loc, mode) =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         string "sqlite3_stmt *stmt;",
         newline,
         newline,
         string "if (sqlite3_prepare_v2(conn->conn, dml, -1, &stmt, NULL) != SQLITE_OK) uw_error(ctx, FATAL, \"Error preparing statement: %s<br />%s\", dml, sqlite3_errmsg(conn->conn));",
         newline,
         newline,
         string "uw_push_cleanup(ctx, (void (*)(void *))sqlite3_finalize, stmt);",
         newline,
         newline,

         dmlCommon {loc = loc, dml = string "dml", mode = mode},

         string "uw_pop_cleanup(ctx);",
         newline]

fun dmlPrepared {loc, id, dml, inputs, mode = mode} =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         p_pre_inputs inputs,
         string "sqlite3_stmt *stmt = conn->p",
         string (Int.toString id),
         string ";",
         newline,
         newline,

         string "if (stmt == NULL) {",
         newline,
         box [string "if (sqlite3_prepare_v2(conn->conn, \"",
              string (Prim.toCString dml),
              string "\", -1, &stmt, NULL) != SQLITE_OK) uw_error(ctx, FATAL, \"Error preparing statement: ",
              string (Prim.toCString dml),
              string "<br />%s\", sqlite3_errmsg(conn->conn));",
              newline,
              string "conn->p",
              string (Int.toString id),
              string " = stmt;",
              newline],
         string "}",
         newline,

         string "uw_push_cleanup(ctx, (void (*)(void *))sqlite3_clear_bindings, stmt);",
         newline,
         string "uw_push_cleanup(ctx, (void (*)(void *))sqlite3_reset, stmt);",
         newline,

         p_inputs loc inputs,
         newline,

         dmlCommon {loc = loc, dml = box [string "\"",
                                          string (Prim.toCString dml),
                                          string "\""], mode = mode},

         string "uw_pop_cleanup(ctx);",
         newline,
         string "uw_pop_cleanup(ctx);",
         newline]

fun nextval {loc, seqE, seqName} =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         string "char *insert = ",
         case seqName of
             SOME s => string ("\"INSERT INTO " ^ s ^ " VALUES (NULL)\"")
           | NONE => box [string "uw_Basis_strcat(ctx, \"INSERT INTO \", uw_Basis_strcat(ctx, ",
                          seqE,
                          string ", \" VALUES ()\"))"],
         string ";",
         newline,
         string "char *delete = ",
         case seqName of
             SOME s => string ("\"DELETE FROM " ^ s ^ "\"")
           | NONE => box [string "uw_Basis_strcat(ctx, \"DELETE FROM \", ",
                          seqE,
                          string ")"],
         string ";",
         newline,
         newline,

         string "if (sqlite3_exec(conn->conn, insert, NULL, NULL, NULL) != SQLITE_OK) uw_error(ctx, FATAL, \"'nextval' INSERT failed: %s\", sqlite3_errmsg(conn->conn));",
         newline,
         string "n = sqlite3_last_insert_rowid(conn->conn);",
         newline,
         string "if (sqlite3_exec(conn->conn, delete, NULL, NULL, NULL) != SQLITE_OK) uw_error(ctx, FATAL, \"'nextval' DELETE failed: %s\", sqlite3_errmsg(conn->conn));",
         newline]

fun nextvalPrepared _ = raise Fail "SQLite.nextvalPrepared called"
fun setval _ = raise Fail "SQLite.setval called"

fun sqlifyString s = "'" ^ String.translate (fn #"'" => "''"
                                              | #"\000" => ""
                                              | ch => str ch)
                                            s ^ "'"

fun p_cast (s, _) = s

fun p_blank _ = "?"

val () = addDbms {name = "sqlite",
                  randomFunction = "RANDOM",
                  header = Config.sqheader,
                  link = "-lsqlite3",
                  init = init,
                  p_sql_type = p_sql_type,
                  query = query,
                  queryPrepared = queryPrepared,
                  dml = dml,
                  dmlPrepared = dmlPrepared,
                  nextval = nextval,
                  nextvalPrepared = nextvalPrepared,
                  setval = setval,
                  sqlifyString = sqlifyString,
                  p_cast = p_cast,
                  p_blank = p_blank,
                  supportsDeleteAs = false,
                  supportsUpdateAs = false,
                  createSequence = fn s => "CREATE TABLE " ^ s ^ " (id INTEGER PRIMARY KEY AUTOINCREMENT)",
                  textKeysNeedLengths = false,
                  supportsNextval = false,
                  supportsNestedPrepared = false,
                  sqlPrefix = "PRAGMA foreign_keys = ON;\nPRAGMA journal_mode = WAL;\n\n",
                  supportsOctetLength = false,
                  trueString = "1",
                  falseString = "0",
                  onlyUnion = false,
                  nestedRelops = false,
                  windowFunctions = false,
                  requiresTimestampDefaults = false,
                  supportsIsDistinctFrom = false,
                  supportsSHA512 = NONE,
                  supportsSimilar = NONE}

end
