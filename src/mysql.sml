(* Copyright (c) 2009-2010, 2015, Adam Chlipala
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

fun p_sql_type t =
    case t of
        Int => "bigint"
      | Float => "double"
      | String => "longtext"
      | Char => "char"
      | Bool => "bool"
      | Time => "timestamp"
      | Blob => "longblob"
      | Channel => "bigint"
      | Client => "int"
      | Nullable t => p_sql_type t

fun p_buffer_type t =
    case t of
        Int => "MYSQL_TYPE_LONGLONG"
      | Float => "MYSQL_TYPE_DOUBLE"
      | String => "MYSQL_TYPE_STRING"
      | Char => "MYSQL_TYPE_STRING"
      | Bool => "MYSQL_TYPE_LONG"
      | Time => "MYSQL_TYPE_TIMESTAMP"
      | Blob => "MYSQL_TYPE_BLOB"
      | Channel => "MYSQL_TYPE_LONGLONG"
      | Client => "MYSQL_TYPE_LONG"
      | Nullable t => p_buffer_type t

fun p_sql_type_base t =
    case t of
        Int => "bigint"
      | Float => "double"
      | String => "longtext"
      | Char => "char"
      | Bool => "tinyint"
      | Time => "timestamp"
      | Blob => "longblob"
      | Channel => "bigint"
      | Client => "int"
      | Nullable t => p_sql_type_base t

val ident = String.translate (fn #"'" => "PRIME"
                               | ch => str ch)

fun checkRel (table, checkNullable) (s, xts) =
    let
        val sl = CharVector.map Char.toLower s
        val sl = if size sl > 1 andalso String.sub (sl, 0) = #"\"" then
                     String.substring (sl, 1, size sl - 2)
                 else
                     sl
        val both = "table_name = '" ^ sl ^ "'"

        val q = "SELECT COUNT(*) FROM information_schema." ^ table ^ " WHERE " ^ both

        val q' = String.concat ["SELECT COUNT(*) FROM information_schema.columns WHERE ",
                                both,
                                " AND (",
                                case String.concatWith " OR "
                                                       (map (fn (x, t) =>
                                                                String.concat ["(LOWER(column_name) = '",
                                                                               Settings.mangleSqlCatalog
                                                                                   (CharVector.map
                                                                                        Char.toLower (ident x)),
                                                                               "' AND data_type ",
                                                                               case p_sql_type_base t of
                                                                                   "bigint" =>
                                                                                   "IN ('bigint', 'int')"
                                                                                 | "longtext" =>
                                                                                   "IN ('longtext', 'varchar')"
                                                                                 | s => "= '" ^ s ^ "'",
                                                                               if checkNullable then
                                                                                   (" AND is_nullable = '"
                                                                                    ^ (if isNotNull t then
                                                                                           "NO"
                                                                                       else
                                                                                           "YES")
                                                                                    ^ "'")
                                                                               else
                                                                                   "",
                                                                               ")"]) xts) of
                                    "" => "FALSE"
                                  | s => s,
                                ")"]

        val q'' = String.concat ["SELECT COUNT(*) FROM information_schema.columns WHERE ",
                                 both,
                                 " AND LOWER(column_name) LIKE '", Settings.mangleSqlCatalog "%'"]
    in
        box [string "if (mysql_query(conn->conn, \"",
             string q,
             string "\")) {",
             newline,
             box [string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Query failed:\\n",
                  string q,
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if ((res = mysql_store_result(conn->conn)) == NULL) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Result store failed:\\n",
                  string q,
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if (mysql_num_fields(res) != 1) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Bad column count:\\n",
                  string q,
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if ((row = mysql_fetch_row(res)) == NULL) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Row fetch failed:\\n",
                  string q,
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if (strcmp(row[0], \"1\")) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Table '",
                  string sl,
                  string "' does not exist.\");",
                  newline],
             string "}",
             newline,
             newline,
             string "mysql_free_result(res);",
             newline,
             newline,

             string "if (mysql_query(conn->conn, \"",
             string q',
             string "\")) {",
             newline,
             box [string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Query failed:\\n",
                  string q',
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if ((res = mysql_store_result(conn->conn)) == NULL) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Result store failed:\\n",
                  string q',
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if (mysql_num_fields(res) != 1) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Bad column count:\\n",
                  string q',
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if ((row = mysql_fetch_row(res)) == NULL) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Row fetch failed:\\n",
                  string q',
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if (strcmp(row[0], \"",
             string (Int.toString (length xts)),
             string "\")) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Table '",
                  string sl,
                  string "' has the wrong column types.\");",
                  newline],
             string "}",
             newline,
             newline,
             string "mysql_free_result(res);",
             newline,
             newline,

             string "if (mysql_query(conn->conn, \"",
             string q'',
             string "\")) {",
             newline,
             box [string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Query failed:\\n",
                  string q'',
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if ((res = mysql_store_result(conn->conn)) == NULL) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Result store failed:\\n",
                  string q'',
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if (mysql_num_fields(res) != 1) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Bad column count:\\n",
                  string q'',
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if ((row = mysql_fetch_row(res)) == NULL) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Row fetch failed:\\n",
                  string q'',
                  string "\");",
                  newline],
             string "}",
             newline,
             newline,

             string "if (strcmp(row[0], \"",
             string (Int.toString (length xts)),
             string "\")) {",
             newline,
             box [string "mysql_free_result(res);",
                  newline,
                  string "mysql_close(conn->conn);",
                  newline,
                  string "uw_error(ctx, FATAL, \"Table '",
                  string sl,
                  string "' has extra columns.\");",
                  newline],
             string "}",
             newline,
             newline,
             string "mysql_free_result(res);",
             newline]
    end

fun init {dbstring, prepared = ss, tables, views, sequences} =
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
                                            string (Prim.toCString s),
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
                  newline,
                  newline,

                  string "if (mysql_library_init(0, NULL, NULL)) {",
                  newline,
                  box [string "fprintf(stderr, \"Could not initialize MySQL library\\n\");",
                       newline,
                       string "exit(1);",
                       newline],
                  string "}",
                  newline],
             string "}",
             newline,
             newline,

             if #persistent (currentProtocol ()) then
                 box [string "static void uw_db_validate(uw_context ctx) {",
                      newline,
                      string "uw_conn *conn = uw_get_db(ctx);",
                      newline,
                      string "MYSQL_RES *res;",
                      newline,
                      string "MYSQL_ROW row;",
                      newline,
                      newline,
                      p_list_sep newline (checkRel ("tables", true)) tables,
                      p_list_sep newline (fn name => checkRel ("tables", true)
                                                              (name, [("id", Settings.Client)])) sequences,
                      p_list_sep newline (checkRel ("views", false)) views,
                      string "}",
                      newline,
                      newline,

                      string "static void uw_db_prepare(uw_context ctx) {",
                      newline,
                      string "uw_conn *conn = uw_get_db(ctx);",
                      newline,
                      string "MYSQL_STMT *stmt;",
                      newline,
                      newline,

                      p_list_sepi newline (fn i => fn (s, _) =>
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
                                                               string "conn->p",
                                                               string (Int.toString i),
                                                               string " = stmt;",
                                                               newline,

                                                               string "if (mysql_stmt_prepare(stmt, \"",
                                                               string (Prim.toCString s),
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
                 box [string "static void uw_db_prepare(uw_context ctx) { }",
                      newline,
                      string "static void uw_db_validate(uw_context ctx) { }"],
             newline,
             newline,

             string "static void uw_db_init(uw_context ctx) {",
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
             string ", CLIENT_MULTI_STATEMENTS) == NULL) {",
             newline,
             box [string "char msg[1024];",
                  newline,
                  string "strncpy(msg, mysql_error(mysql), 1024);",
                  newline,
                  string "msg[1023] = 0;",
                  newline,
                  string "mysql_close(mysql);",
                  newline,
                  string "uw_error(ctx, FATAL, ",
                  string "\"Connection to MySQL server failed: %s\", msg);"],
             newline,
             string "}",
             newline,
             newline,
             string "if (mysql_set_character_set(mysql, \"utf8\")) {",
             newline,
             box [string "char msg[1024];",
                  newline,
                  string "strncpy(msg, mysql_error(mysql), 1024);",
                  newline,
                  string "msg[1023] = 0;",
                  newline,
                  string "mysql_close(mysql);",
                  newline,
                  string "uw_error(ctx, FATAL, ",
                  string "\"Error setting UTF-8 character set for MySQL connection: %s\", msg);"],
             newline,
             string "}",
             newline,
             newline,
             string "conn = calloc(1, sizeof(uw_conn));",
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

             string "static void uw_db_close(uw_context ctx) {",
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

             string "static int uw_db_begin(uw_context ctx, int could_write) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             newline,
             string "return mysql_query(conn->conn, \"SET TRANSACTION ISOLATION LEVEL SERIALIZABLE; BEGIN\") ? 1 : (mysql_next_result(conn->conn), 0);",
             newline,
             string "}",
             newline,
             newline,

             string "static int uw_db_commit(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             string "return mysql_commit(conn->conn);",
             newline,
             string "}",
             newline,
             newline,

             string "static int uw_db_rollback(uw_context ctx) {",
             newline,
             string "uw_conn *conn = uw_get_db(ctx);",
             newline,
             string "return mysql_rollback(conn->conn);",
             newline,
             string "}",
             newline,
             newline]
    end

fun p_getcol {loc, wontLeakStrings = _, col = i, typ = t} =
    let
        fun getter t =
            case t of
                String => box [string "({",
                               newline,
                               string "uw_Basis_string s = uw_malloc(ctx, length",
                               string (Int.toString i),
                               string " + 1);",
                               newline,
                               string "out[",
                               string (Int.toString i),
                               string "].buffer = s;",
                               newline,
                               string "out[",
                               string (Int.toString i),
                               string "].buffer_length = length",
                               string (Int.toString i),
                               string " + 1;",
                               newline,
                               string "mysql_stmt_fetch_column(stmt, &out[",
                               string (Int.toString i),
                               string "], ",
                               string (Int.toString i),
                               string ", 0);",
                               newline,
                               string "s[length",
                               string (Int.toString i),
                               string "] = 0;",
                               newline,
                               string "s;",
                               newline,
                               string "})"]
              | Blob => box [string "({",
                             newline,
                             string "uw_Basis_blob b = {length",
                             string (Int.toString i),
                             string ", uw_malloc(ctx, length",
                             string (Int.toString i),
                             string ")};",
                             newline,
                             string "out[",
                             string (Int.toString i),
                             string "].buffer = b.data;",
                             newline,
                             string "out[",
                             string (Int.toString i),
                             string "].buffer_length = length",
                             string (Int.toString i),
                             string ";",
                             newline,
                             string "mysql_stmt_fetch_column(stmt, &out[",
                             string (Int.toString i),
                             string "], ",
                             string (Int.toString i),
                             string ", 0);",
                             newline,
                             string "b;",
                             newline,
                             string "})"]
              | Time => box [string "({",
                             string "MYSQL_TIME *mt = &buffer",
                             string (Int.toString i),
                             string ";",
                             newline,
                             newline,
                             string "struct tm t = {mt->second, mt->minute, mt->hour, mt->day, mt->month-1, mt->year - 1900, 0, 0, -1};",
                             newline,
                             string "uw_Basis_time res = {mktime(&t), 0};",
                             newline,
                             string "res;",
                             newline,
                             string "})"]
              | Channel => box [string "({",
                                string "uw_Basis_channel ch = {buffer",
                                string (Int.toString i),
                                string " >> 32, buffer",
                                string (Int.toString i),
                                string " & 0xFFFFFFFF};",
                                newline,
                                string "ch;",
                                newline,
                                string "})"]
              | _ => box [string "buffer",
                          string (Int.toString i)]
    in
        case t of
            Nullable t => box [string "(is_null",
                               string (Int.toString i),
                               string " ? NULL : ",
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
          | _ => box [string "(is_null",
                      string (Int.toString i),
                      string " ? ",
                      box [string "({",
                           string (p_sql_ctype t),
                           space,
                           string "tmp;",
                           newline,
                           string "uw_error(ctx, FATAL, \"Unexpectedly NULL field #",
                           string (Int.toString i),
                           string "\");",
                           newline,
                           string "tmp;",
                           newline,
                           string "})"],
                      string " : ",
                      getter t,
                      string ")"]
    end

fun queryCommon {loc, query, cols, doCols} =
    box [string "int n, r;",
         newline,
         string "MYSQL_BIND out[",
         string (Int.toString (length cols)),
         string "];",
         newline,
         p_list_sepi (box []) (fn i => fn t =>
                                          let
                                              fun buffers t =
                                                  case t of
                                                      String => box [string "unsigned long length",
                                                                     string (Int.toString i),
                                                                     string ";",
                                                                     newline]
                                                    | Blob => box [string "unsigned long length",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | Time => box [string "MYSQL_TIME buffer",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | Channel => box [string "unsigned long long buffer",
                                                                      string (Int.toString i),
                                                                      string ";",
                                                                      newline]
                                                    | _ => box [string (p_sql_ctype t),
                                                                space,
                                                                string "buffer",
                                                                string (Int.toString i),
                                                                string ";",
                                                                newline]
                                          in
                                              box [string "bool is_null",
                                                   string (Int.toString i),
                                                   string ";",
                                                   newline,
                                                   case t of
                                                       Nullable t => buffers t
                                                     | _ => buffers t,
                                                   newline]
                                          end) cols,
         newline,

         string "memset(out, 0, sizeof out);",
         newline,
         p_list_sepi (box []) (fn i => fn t =>
                                          let
                                              fun buffers t =
                                                  case t of
                                                      String => box [string "out[",
                                                                     string (Int.toString i),
                                                                     string "].length = &length",
                                                                     string (Int.toString i),
                                                                     string ";",
                                                                     newline]
                                                    | Char => box [string "out[",
                                                                   string (Int.toString i),
                                                                   string "].buffer_length = 1;",
                                                                   newline,
                                                                   string "out[",
                                                                   string (Int.toString i),
                                                                   string "].buffer = &buffer",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | Blob => box [string "out[",
                                                                   string (Int.toString i),
                                                                   string "].length = &length",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | _ => box [string "out[",
                                                                string (Int.toString i),
                                                                string "].buffer = &buffer",
                                                                string (Int.toString i),
                                                                string ";",
                                                                newline]
                                          in
                                              box [string "out[",
                                                   string (Int.toString i),
                                                   string "].buffer_type = ",
                                                   string (p_buffer_type t),
                                                   string ";",
                                                   newline,
                                                   string "out[",
                                                   string (Int.toString i),
                                                   string "].is_null = &is_null",
                                                   string (Int.toString i),
                                                   string ";",
                                                   newline,

                                                   case t of
                                                       Nullable t => buffers t
                                                     | _ => buffers t,
                                                  newline]
                                          end) cols,
         newline,

         string "if (mysql_stmt_reset(stmt)) {",
         box [newline,
              string "if (mysql_errno(conn->conn) == 2006) uw_try_reconnecting_and_restarting(ctx);",
              newline,
              string "uw_error(ctx, FATAL, \"",
              string (ErrorMsg.spanToString loc),
              string ": Error resetting statement: %s\\n%s\", ",
              query,
              string ", mysql_error(conn->conn));",
              newline],
         string "}",
         newline,
         newline,

         string "if (mysql_stmt_execute(stmt)) {",
         newline,
         box [string "if (mysql_errno(conn->conn) == 1213)",
              newline,
              box [string "uw_error(ctx, UNLIMITED_RETRY, \"Deadlock detected\");",
                   newline],
              newline,
              string "uw_error(ctx, FATAL, \"",
              string (ErrorMsg.spanToString loc),
              string ": Error executing query: %s\\n%s\", ",
              query,
              string ", mysql_error(conn->conn));",
              newline],
         string "}",
         newline,
         newline,

         string "if (mysql_stmt_bind_result(stmt, out)) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": Error binding query result: %s\\n%s\", ",
         query,
         string ", mysql_error(conn->conn));",
         newline,
         newline,

         string "if (mysql_stmt_store_result(stmt)) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": Error storing query result: %s\\n%s\", ",
         query,
         string ", mysql_error(conn->conn));",
         newline,
         newline,

         string "uw_end_region(ctx);",
         newline,
         string "while (1) {",
         newline,
         string "r = mysql_stmt_fetch(stmt);",
         newline,
         string "if (r != 0 && r != MYSQL_DATA_TRUNCATED) break;",
         newline,
         doCols p_getcol,
         string "}",
         newline,
         newline,

         string "if (r == 1) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": query result fetching failed: %s\\n%s\", ",
         query,
         string ", mysql_error(conn->conn));",
         newline,
         newline,

         string "if (mysql_stmt_reset(stmt)) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": Error resetting statement: %s\\n%s\", ",
         query,
         string ", mysql_error(conn->conn));",
         newline,
         newline]

fun query {loc, cols, doCols} =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         string "MYSQL_STMT *stmt = mysql_stmt_init(conn->conn);",
         newline,
         string "if (stmt == NULL) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": can't allocate temporary prepared statement\");",
         newline,
         string "uw_push_cleanup(ctx, (void (*)(void *))mysql_stmt_close, stmt);",
         newline,
         string "if (mysql_stmt_prepare(stmt, query, strlen(query))) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": error preparing statement: %s\\n%s\", query, mysql_error(conn->conn));",
         newline,
         newline,

         queryCommon {loc = loc, cols = cols, doCols = doCols, query = string "query"},

         string "uw_pop_cleanup(ctx);",
         newline]

fun queryPrepared {loc, id, query, inputs, cols, doCols, nested} =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         string "MYSQL_BIND in[",
         string (Int.toString (length inputs)),
         string "];",
         newline,
         p_list_sepi (box []) (fn i => fn t =>
                                          let
                                              fun buffers t =
                                                  case t of
                                                      String => box [string "unsigned long in_length",
                                                                     string (Int.toString i),
                                                                     string ";",
                                                                     newline]
                                                    | Blob => box [string "unsigned long in_length",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | Time => box [string "MYSQL_TIME in_buffer",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | _ => box []
                                          in
                                              box [case t of
                                                       Nullable t => box [string "bool in_is_null",
                                                                          string (Int.toString i),
                                                                          string ";",
                                                                          newline,
                                                                          buffers t]
                                                     | _ => buffers t,
                                                   newline]
                                          end) inputs,

         if nested then
             box [string "MYSQL_STMT *stmt;",
                  newline]
         else
             box [string "MYSQL_STMT *stmt = conn->p",
                  string (Int.toString id),
                  string ";",
                  newline,
                  newline,

                  string "if (stmt == NULL) {",
                  newline],

         box [string "stmt = mysql_stmt_init(conn->conn);",
              newline,
              string "if (stmt == NULL) uw_error(ctx, FATAL, \"Out of memory allocating prepared statement\");",
              newline,
              if nested then
                  box [string "uw_push_cleanup(ctx, (void (*)(void *))mysql_stmt_close, stmt);",
                       newline]
              else
                  box [],
              string "if (mysql_stmt_prepare(stmt, \"",
              string (Prim.toCString query),
              string "\", ",
              string (Int.toString (size query)),
              string ")) {",
              newline,
              box [string "char msg[1024];",
                   newline,
                   string "strncpy(msg, mysql_stmt_error(stmt), 1024);",
                   newline,
                   string "msg[1023] = 0;",
                   newline,
                   if nested then
                       box []
                   else
                       box [string "mysql_stmt_close(stmt);",
                            newline],
                   string "uw_error(ctx, FATAL, \"Error preparing statement: %s\", msg);",
                   newline],
              string "}",
              newline,
              if nested then
                  box []
              else
                  box [string "conn->p",
                       string (Int.toString id),
                       string " = stmt;",
                       newline]],
         if nested then
             box []
         else
             box [string "}",
                  newline],
         newline,

         string "memset(in, 0, sizeof in);",
         newline,
         p_list_sepi (box []) (fn i => fn t =>
                                          let
                                              fun buffers t =
                                                  case t of
                                                      String => box [string "in[",
                                                                     string (Int.toString i),
                                                                     string "].buffer = arg",
                                                                     string (Int.toString (i + 1)),
                                                                     string ";",
                                                                     newline,
                                                                     string "in_length",
                                                                     string (Int.toString i),
                                                                     string "= in[",
                                                                     string (Int.toString i),
                                                                     string "].buffer_length = strlen(arg",
                                                                     string (Int.toString (i + 1)),
                                                                     string ");",
                                                                     newline,
                                                                     string "in[",
                                                                     string (Int.toString i),
                                                                     string "].length = &in_length",
                                                                     string (Int.toString i),
                                                                     string ";",
                                                                     newline]
                                                    | Char => box [string "in[",
                                                                   string (Int.toString i),
                                                                   string "].buffer = &arg",
                                                                   string (Int.toString (i + 1)),
                                                                   string ";",
                                                                   newline,
                                                                   string "in[",
                                                                   string (Int.toString i),
                                                                   string "].buffer_length = 1;",
                                                                   newline]
                                                    | Blob => box [string "in[",
                                                                   string (Int.toString i),
                                                                   string "].buffer = arg",
                                                                   string (Int.toString (i + 1)),
                                                                   string ".data;",
                                                                   newline,
                                                                   string "in_length",
                                                                   string (Int.toString i),
                                                                   string "= in[",
                                                                   string (Int.toString i),
                                                                   string "].buffer_length = arg",
                                                                   string (Int.toString (i + 1)),
                                                                   string ".size;",
                                                                   newline,
                                                                   string "in[",
                                                                   string (Int.toString i),
                                                                   string "].length = &in_length",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | Time =>
                                                      let
                                                          fun oneField dst src =
                                                              box [string "in_buffer",
                                                                   string (Int.toString i),
                                                                   string ".",
                                                                   string dst,
                                                                   string " = tms.tm_",
                                                                   string src,
                                                                   string ";",
                                                                   newline]
                                                      in
                                                          box [string "({",
                                                               newline,
                                                               string "struct tm tms;",
                                                               newline,
                                                               string "if (localtime_r(&arg",
                                                               string (Int.toString (i + 1)),
                                                               string ".seconds, &tms) == NULL) uw_error(ctx, FATAL, \"",
                                                               string (ErrorMsg.spanToString loc),
                                                               string ": error converting to MySQL time\");",
                                                               newline,
                                                               oneField "year" "year + 1900",
                                                               box [string "in_buffer",
                                                                    string (Int.toString i),
                                                                    string ".month = tms.tm_mon + 1;",
                                                                    newline],
                                                               oneField "day" "mday",
                                                               oneField "hour" "hour",
                                                               oneField "minute" "min",
                                                               oneField "second" "sec",
                                                               newline,
                                                               string "in[",
                                                               string (Int.toString i),
                                                               string "].buffer = &in_buffer",
                                                               string (Int.toString i),
                                                               string ";",
                                                               newline,
                                                               string "});",
                                                               newline]
                                                      end
                                                    | Channel => box [string "in_buffer",
                                                                      string (Int.toString i),
                                                                      string " = ((unsigned long long)arg",
                                                                      string (Int.toString (i + 1)),
                                                                      string ".cli << 32) | arg",
                                                                      string (Int.toString (i + 1)),
                                                                      string ".chn;",
                                                                      newline,
                                                                      string "in[",
                                                                      string (Int.toString i),
                                                                      string "].buffer = &in_buffer",
                                                                      string (Int.toString i),
                                                                      string ";",
                                                                      newline]

                                                    | _ => box [string "in[",
                                                                string (Int.toString i),
                                                                string "].buffer = &arg",
                                                                string (Int.toString (i + 1)),
                                                                string ";",
                                                                newline]
                                          in
                                              box [string "in[",
                                                   string (Int.toString i),
                                                   string "].buffer_type = ",
                                                   string (p_buffer_type t),
                                                   string ";",
                                                   newline,

                                                   case t of
                                                       Nullable t => box [string "in[",
                                                                          string (Int.toString i),
                                                                          string "].is_null = &in_is_null",
                                                                          string (Int.toString i),
                                                                          string ";",
                                                                          newline,
                                                                          string "if (arg",
                                                                          string (Int.toString (i + 1)),
                                                                          string " == NULL) {",
                                                                          newline,
                                                                          box [string "in_is_null",
                                                                               string (Int.toString i),
                                                                               string " = 1;",
                                                                               newline],
                                                                          string "} else {",
                                                                          box [case t of
                                                                                   String => box []
                                                                                 | _ =>
                                                                                   box [string (p_sql_ctype t),
                                                                                        space,
                                                                                        string "tmp = *arg",
                                                                                        string (Int.toString (i + 1)),
                                                                                        string ";",
                                                                                        newline,
                                                                                        string (p_sql_ctype t),
                                                                                        space,
                                                                                        string "arg",
                                                                                        string (Int.toString (i + 1)),
                                                                                        string " = tmp;",
                                                                                        newline],
                                                                               string "in_is_null",
                                                                               string (Int.toString i),
                                                                               string " = 0;",
                                                                               newline,
                                                                               buffers t,
                                                                               newline],
                                                                          string "}",
                                                                          newline]

                                                     | _ => buffers t,
                                                   newline]
                                          end) inputs,
         newline,

         string "if (mysql_stmt_bind_param(stmt, in)) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": error binding parameters\");",
         newline,

         queryCommon {loc = loc, cols = cols, doCols = doCols, query = box [string "\"",
                                                                            string (Prim.toCString query),
                                                                            string "\""]},

         if nested then
             box [string "uw_pop_cleanup(ctx);",
                  newline]
         else
             box []]

fun dmlCommon {loc, dml, mode} =
    box [string "if (mysql_stmt_execute(stmt)) {",
         box [string "if (mysql_errno(conn->conn) == 2006) uw_try_reconnecting_and_restarting(ctx);",
              newline,
              string "if (mysql_errno(conn->conn) == 1213)",
              newline,
              box [string "uw_error(ctx, UNLIMITED_RETRY, \"Deadlock detected\");",
                   newline],
              newline,
              case mode of
                  Settings.Error => box [string "uw_error(ctx, FATAL, \"",
                                         string (ErrorMsg.spanToString loc),
                                         string ": Error executing DML: %s\\n%s\", ",
                                         dml,
                                         string ", mysql_error(conn->conn));"]
                | Settings.None => string "uw_set_error_message(ctx, mysql_error(conn->conn));",
              newline],
         string "}",
         newline]

fun dml (loc, mode) =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         string "MYSQL_STMT *stmt = mysql_stmt_init(conn->conn);",
         newline,
         string "if (stmt == NULL) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": can't allocate temporary prepared statement\");",
         newline,
         string "uw_push_cleanup(ctx, (void (*)(void *))mysql_stmt_close, stmt);",
         newline,
         string "if (mysql_stmt_prepare(stmt, dml, strlen(dml))) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": error preparing statement: %s\\n%s\", dml, mysql_error(conn->conn));",
         newline,
         newline,

         dmlCommon {loc = loc, dml = string "dml", mode = mode},

         string "uw_pop_cleanup(ctx);",
         newline]

fun dmlPrepared {loc, id, dml, inputs, mode} =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         string "MYSQL_BIND in[",
         string (Int.toString (length inputs)),
         string "];",
         newline,
         p_list_sepi (box []) (fn i => fn t =>
                                          let
                                              fun buffers t =
                                                  case t of
                                                      String => box [string "unsigned long in_length",
                                                                     string (Int.toString i),
                                                                     string ";",
                                                                     newline]
                                                    | Blob => box [string "unsigned long in_length",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | Time => box [string "MYSQL_TIME in_buffer",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | Channel => box [string "unsigned long long in_buffer",
                                                                      string (Int.toString i),
                                                                      string ";",
                                                                      newline]
                                                    | _ => box []
                                          in
                                              box [case t of
                                                       Nullable t => box [string "bool in_is_null",
                                                                          string (Int.toString i),
                                                                          string ";",
                                                                          newline,
                                                                          buffers t]
                                                     | _ => buffers t,
                                                   newline]
                                          end) inputs,
         string "MYSQL_STMT *stmt = conn->p",
         string (Int.toString id),
         string ";",
         newline,
         newline,

         string "if (stmt == NULL) {",
         newline,
         box [string "stmt = mysql_stmt_init(conn->conn);",
              newline,
              string "if (stmt == NULL) uw_error(ctx, FATAL, \"Out of memory allocating prepared statement\");",
              newline,
              string "if (mysql_stmt_prepare(stmt, \"",
              string (Prim.toCString dml),
              string "\", ",
              string (Int.toString (size dml)),
              string ")) {",
              newline,
              box [string "char msg[1024];",
                   newline,
                   string "strncpy(msg, mysql_stmt_error(stmt), 1024);",
                   newline,
                   string "msg[1023] = 0;",
                   newline,
                   string "uw_error(ctx, FATAL, \"Error preparing statement: %s\", msg);",
                   newline],
              string "}",
              newline,
              string "conn->p",
              string (Int.toString id),
              string " = stmt;",
              newline],
         string "}",
         newline,
         newline,

         string "memset(in, 0, sizeof in);",
         newline,
         p_list_sepi (box []) (fn i => fn t =>
                                          let
                                              fun buffers t =
                                                  case t of
                                                      String => box [string "in[",
                                                                     string (Int.toString i),
                                                                     string "].buffer = arg",
                                                                     string (Int.toString (i + 1)),
                                                                     string ";",
                                                                     newline,
                                                                     string "in_length",
                                                                     string (Int.toString i),
                                                                     string "= in[",
                                                                     string (Int.toString i),
                                                                     string "].buffer_length = strlen(arg",
                                                                     string (Int.toString (i + 1)),
                                                                     string ");",
                                                                     newline,
                                                                     string "in[",
                                                                     string (Int.toString i),
                                                                     string "].length = &in_length",
                                                                     string (Int.toString i),
                                                                     string ";",
                                                                     newline]
                                                    | Blob => box [string "in[",
                                                                   string (Int.toString i),
                                                                   string "].buffer = arg",
                                                                   string (Int.toString (i + 1)),
                                                                   string ".data;",
                                                                   newline,
                                                                   string "in_length",
                                                                   string (Int.toString i),
                                                                   string "= in[",
                                                                   string (Int.toString i),
                                                                   string "].buffer_length = arg",
                                                                   string (Int.toString (i + 1)),
                                                                   string ".size;",
                                                                   newline,
                                                                   string "in[",
                                                                   string (Int.toString i),
                                                                   string "].length = &in_length",
                                                                   string (Int.toString i),
                                                                   string ";",
                                                                   newline]
                                                    | Time =>
                                                      let
                                                          fun oneField dst src =
                                                              box [string "in_buffer",
                                                                   string (Int.toString i),
                                                                   string ".",
                                                                   string dst,
                                                                   string " = tms.tm_",
                                                                   string src,
                                                                   string ";",
                                                                   newline]
                                                      in
                                                          box [string "({",
                                                               newline,
                                                               string "struct tm tms;",
                                                               newline,
                                                               string "if (localtime_r(&arg",
                                                               string (Int.toString (i + 1)),
                                                               string ".seconds, &tms) == NULL) uw_error(ctx, FATAL, \"",
                                                               string (ErrorMsg.spanToString loc),
                                                               string ": error converting to MySQL time\");",
                                                               newline,
                                                               oneField "year" "year + 1900",
                                                               oneField "month" "mon + 1",
                                                               oneField "day" "mday",
                                                               oneField "hour" "hour",
                                                               oneField "minute" "min",
                                                               oneField "second" "sec",
                                                               newline,
                                                               string "in[",
                                                               string (Int.toString i),
                                                               string "].buffer = &in_buffer",
                                                               string (Int.toString i),
                                                               string ";",
                                                               newline,
                                                               string "});",
                                                               newline]
                                                      end
                                                    | Channel => box [string "in_buffer",
                                                                      string (Int.toString i),
                                                                      string " = ((unsigned long long)arg",
                                                                      string (Int.toString (i + 1)),
                                                                      string ".cli << 32) | arg",
                                                                      string (Int.toString (i + 1)),
                                                                      string ".chn;",
                                                                      newline,
                                                                      string "in[",
                                                                      string (Int.toString i),
                                                                      string "].buffer = &in_buffer",
                                                                      string (Int.toString i),
                                                                      string ";",
                                                                      newline]

                                                    | _ => box [string "in[",
                                                                string (Int.toString i),
                                                                string "].buffer = &arg",
                                                                string (Int.toString (i + 1)),
                                                                string ";",
                                                                newline]
                                          in
                                              box [string "in[",
                                                   string (Int.toString i),
                                                   string "].buffer_type = ",
                                                   string (p_buffer_type t),
                                                   string ";",
                                                   newline,

                                                   case t of
                                                       Channel => box [string "in[",
                                                                       string (Int.toString i),
                                                                       string "].is_unsigned = 1;",
                                                                       newline]
                                                     | _ => box [],

                                                   case t of
                                                       Nullable t => box [string "in[",
                                                                          string (Int.toString i),
                                                                          string "].is_null = &in_is_null",
                                                                          string (Int.toString i),
                                                                          string ";",
                                                                          newline,
                                                                          string "if (arg",
                                                                          string (Int.toString (i + 1)),
                                                                          string " == NULL) {",
                                                                          newline,
                                                                          box [string "in_is_null",
                                                                               string (Int.toString i),
                                                                               string " = 1;",
                                                                               newline],
                                                                          string "} else {",
                                                                          box [case t of
                                                                                   String => box []
                                                                                 | _ =>
                                                                                   box [string (p_sql_ctype t),
                                                                                        space,
                                                                                        string "tmp = *arg",
                                                                                        string (Int.toString (i + 1)),
                                                                                        string ";",
                                                                                        newline,
                                                                                        string (p_sql_ctype t),
                                                                                        space,
                                                                                        string "arg",
                                                                                        string (Int.toString (i + 1)),
                                                                                        string " = tmp;",
                                                                                        newline],
                                                                               string "in_is_null",
                                                                               string (Int.toString i),
                                                                               string " = 0;",
                                                                               newline,
                                                                               buffers t,
                                                                               newline],
                                                                          string "}",
                                                                          newline]

                                                     | _ => buffers t,
                                                   newline]
                                          end) inputs,
         newline,

         string "if (mysql_stmt_bind_param(stmt, in)) uw_error(ctx, FATAL, \"",
         string (ErrorMsg.spanToString loc),
         string ": error binding parameters\");",
         newline,

         dmlCommon {loc = loc, dml = box [string "\"",
                                          string (Prim.toCString dml),
                                          string "\""], mode = mode}]

fun nextval {loc, seqE, seqName} =
    box [string "uw_conn *conn = uw_get_db(ctx);",
         newline,
         string "char *insert = ",
         case seqName of
             SOME s => string ("\"INSERT INTO " ^ s ^ " VALUES ()\"")
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

         string "if (mysql_query(conn->conn, insert)) {",
         box [newline,
              string "if (mysql_errno(conn->conn) == 2006) uw_try_reconnecting_and_restarting(ctx);",
              newline,
              string "uw_error(ctx, FATAL, \"'nextval' INSERT failed\");",
              newline],
         string "}",
         newline,
         string "n = mysql_insert_id(conn->conn);",
         newline,
         string "if (mysql_query(conn->conn, delete)) uw_error(ctx, FATAL, \"'nextval' DELETE failed\");",
         newline]

fun nextvalPrepared _ = raise Fail "MySQL.nextvalPrepared called"

fun setval _ = raise Fail "MySQL.setval called"

fun sqlifyString s = "'" ^ String.translate (fn #"'" => "\\'"
                                              | #"\\" => "\\\\"
                                              | ch =>
                                                if Char.isPrint ch then
                                                    str ch
                                                else
                                                    (ErrorMsg.error
                                                         "Non-printing character found in SQL string literal";
                                                     ""))
                                            (Prim.toCString s) ^ "'"

fun p_cast (s, _) = s

fun p_blank _ = "?"

val () = addDbms {name = "mysql",
                  header = Config.msheader,
                  randomFunction = "RAND",
                  link = "-lmysqlclient",
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
                  createSequence = fn s => "CREATE TABLE " ^ s ^ " (uw_id INTEGER PRIMARY KEY AUTO_INCREMENT)",
                  textKeysNeedLengths = true,
                  supportsNextval = false,
                  supportsNestedPrepared = false,
                  sqlPrefix = "",
                  supportsOctetLength = true,
                  trueString = "TRUE",
                  falseString = "FALSE",
                  onlyUnion = true,
                  nestedRelops = false,
                  windowFunctions = false,
                  requiresTimestampDefaults = true,
                  supportsIsDistinctFrom = true,
                  supportsSHA512 = SOME {InitializeDb = "",
                                         GenerateHash = fn name => "SHA2(" ^ name ^ ", 512)"},
                  supportsSimilar = NONE}

end
