(* Copyright (c) 2008, Adam Chlipala
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

(* Pretty-printing C jr. *)

structure CjrPrint :> CJR_PRINT = struct

open Print.PD
open Print

open Cjr

val dummyt = (TRecord 0, ErrorMsg.dummySpan)

structure E = CjrEnv
structure EM = ErrorMsg

structure SK = struct
type ord_key = string
val compare = String.compare
end

structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)
structure IS = IntBinarySet

structure CM = BinaryMapFn(struct
                           type ord_key = char
                           val compare = Char.compare
                           end)

val debug = ref false

val dummyTyp = (TDatatype (Enum, 0, ref []), ErrorMsg.dummySpan)

fun p_typ' par env (t, loc) =
    case t of
        TFun (t1, t2) => parenIf par (box [p_typ' true env t2,
                                           space,
                                           string "(*)",
                                           space,
                                           string "(",
                                           p_typ env t1,
                                           string ")"])
      | TRecord i => box [string "struct",
                          space,
                          string "__lws_",
                          string (Int.toString i)]
      | TDatatype (Enum, n, _) =>
        (box [string "enum",
              space,
              string ("__lwe_" ^ #1 (E.lookupDatatype env n) ^ "_" ^ Int.toString n)]
         handle CjrEnv.UnboundNamed _ => string ("__lwd_UNBOUND__" ^ Int.toString n))
      | TDatatype (Option, n, xncs) =>
        (case ListUtil.search #3 (!xncs) of
             NONE => raise Fail "CjrPrint: TDatatype marked Option has no constructor with an argument"
           | SOME t =>
             case #1 t of
                 TDatatype _ => p_typ' par env t
               | TFfi ("Basis", "string") => p_typ' par env t
               | _ => box [p_typ' par env t,
                           string "*"])
      | TDatatype (Default, n, _) =>
        (box [string "struct",
              space,
              string ("__lwd_" ^ #1 (E.lookupDatatype env n) ^ "_" ^ Int.toString n ^ "*")]
         handle CjrEnv.UnboundNamed _ => string ("__lwd_UNBOUND__" ^ Int.toString n))
      | TFfi (m, x) => box [string "lw_", string m, string "_", string x]

and p_typ env = p_typ' false env

fun p_rel env n = string ("__lwr_" ^ #1 (E.lookupERel env n) ^ "_" ^ Int.toString (E.countERels env - n - 1))
    handle CjrEnv.UnboundRel _ => string ("__lwr_UNBOUND_" ^ Int.toString (E.countERels env - n - 1))

fun p_enamed env n =
    string ("__lwn_" ^ #1 (E.lookupENamed env n) ^ "_" ^ Int.toString n)
    handle CjrEnv.UnboundNamed _ => string ("__lwn_UNBOUND_" ^ Int.toString n)

fun p_con_named env n =
    string ("__lwc_" ^ #1 (E.lookupConstructor env n) ^ "_" ^ Int.toString n)
    handle CjrEnv.UnboundNamed _ => string ("__lwc_UNBOUND_" ^ Int.toString n)

fun p_pat_preamble env (p, _) =
    case p of
        PWild => (box [],
                  env)
      | PVar (x, t) => (box [p_typ env t,
                             space,
                             string "__lwr_",
                             string x,
                             string "_",
                             string (Int.toString (E.countERels env)),
                             string ";",
                             newline],
                        E.pushERel env x t)
      | PPrim _ => (box [], env)
      | PCon (_, _, NONE) => (box [], env)
      | PCon (_, _, SOME p) => p_pat_preamble env p
      | PRecord xps =>
        foldl (fn ((_, p, _), (pp, env)) =>
                  let
                      val (pp', env) = p_pat_preamble env p
                  in
                      (box [pp', pp], env)
                  end) (box [], env) xps

fun p_patCon env pc =
    case pc of
        PConVar n => p_con_named env n
      | PConFfi {mod = m, con, ...} => string ("lw_" ^ m ^ "_" ^ con)

fun p_pat (env, exit, depth) (p, _) =
    case p of
        PWild =>
        (box [], env)
      | PVar (x, t) =>
        (box [string "__lwr_",
              string x,
              string "_",
              string (Int.toString (E.countERels env)),
              space,
              string "=",
              space,
              string "disc",
              string (Int.toString depth),
              string ";"],
         E.pushERel env x t)
      | PPrim (Prim.Int n) =>
        (box [string "if",
              space,
              string "(disc",
              string (Int.toString depth),
              space,
              string "!=",
              space,
              Prim.p_t (Prim.Int n),
              string ")",
              space,
              exit],
         env)
      | PPrim (Prim.String s) =>
        (box [string "if",
              space,
              string "(strcmp(disc",
              string (Int.toString depth),
              string ",",
              space,
              Prim.p_t (Prim.String s),
              string "))",
              space,
              exit],
         env)
      | PPrim _ => raise Fail "CjrPrint: Disallowed PPrim primitive"

      | PCon (dk, pc, po) =>
        let
            val (p, env) =
                case po of
                    NONE => (box [], env)
                  | SOME p =>
                    let
                        val (p, env) = p_pat (env, exit, depth + 1) p

                        val (x, to) = case pc of
                                          PConVar n =>
                                          let
                                              val (x, to, _) = E.lookupConstructor env n
                                          in
                                              ("lw_" ^ x, to)
                                          end
                                        | PConFfi {mod = m, con, arg, ...} =>
                                          ("lw_" ^ m ^ "_" ^ con, arg)

                        val t = case to of
                                    NONE => raise Fail "CjrPrint: Constructor mismatch"
                                  | SOME t => t
                    in
                        (box [string "{",
                              newline,
                              p_typ env t,
                              space,
                              string "disc",
                              string (Int.toString (depth + 1)),
                              space,
                              string "=",
                              space,
                              case dk of
                                  Enum => raise Fail "CjrPrint: Looking at argument of no-argument constructor"
                                | Default => box [string "disc",
                                                  string (Int.toString depth),
                                                  string "->data.",
                                                  string x]
                                | Option =>
                                  case #1 t of
                                      TDatatype _ => box [string "disc",
                                                          string (Int.toString depth)]
                                    | TFfi ("Basis", "string") => box [string "disc",
                                                                       string (Int.toString depth)]
                                    | _ => box [string "*disc",
                                                string (Int.toString depth)],
                              string ";",
                              newline,
                              p,
                              newline,
                              string "}"],
                         env)
                    end
        in
            (box [string "if",
                  space,
                  string "(disc",
                  string (Int.toString depth),
                  case (dk, po) of
                      (Enum, _) => box [space,
                                        string "!=",
                                        space,
                                        p_patCon env pc]
                    | (Default, _) => box [string "->tag",
                                           space,
                                           string "!=",
                                           space,
                                           p_patCon env pc]
                    | (Option, NONE) => box [space,
                                             string "!=",
                                             space,
                                             string "NULL"]
                    | (Option, SOME _) => box [space,
                                               string "==",
                                               space,
                                               string "NULL"],
                  string ")",
                  space,
                  exit,
                  newline,
                  p],
             env)
        end

      | PRecord xps =>
        let
            val (xps, env) =
                ListUtil.foldlMap (fn ((x, p, t), env) =>
                                      let
                                          val (p, env) = p_pat (env, exit, depth + 1) p

                                          val p = box [string "{",
                                                       newline,
                                                       p_typ env t,
                                                       space,
                                                       string "disc",
                                                       string (Int.toString (depth + 1)),
                                                       space,
                                                       string "=",
                                                       space,
                                                       string "disc",
                                                       string (Int.toString depth),
                                                       string ".__lwf_",
                                                       string x,
                                                       string ";",
                                                       newline,
                                                       p,
                                                       newline,
                                                       string "}"]
                                      in
                                          (p, env)
                                      end) env xps
        in
            (p_list_sep newline (fn x => x) xps,
             env)
        end

local
    val count = ref 0
in
fun newGoto () =
    let
        val r = !count
    in
        count := r + 1;
        string ("L" ^ Int.toString r)
    end
end

fun patConInfo env pc =
    case pc of
        PConVar n =>
        let
            val (x, _, dn) = E.lookupConstructor env n
            val (dx, _) = E.lookupDatatype env dn
        in
            ("__lwd_" ^ dx ^ "_" ^ Int.toString dn,
             "__lwc_" ^ x ^ "_" ^ Int.toString n,
             "lw_" ^ x)
        end
      | PConFfi {mod = m, datatyp, con, ...} =>
        ("lw_" ^ m ^ "_" ^ datatyp,
         "lw_" ^ m ^ "_" ^ con,
         "lw_" ^ con)

fun p_exp' par env (e, loc) =
    case e of
        EPrim p => Prim.p_t p
      | ERel n => p_rel env n
      | ENamed n => p_enamed env n
      | ECon (Enum, pc, _) => p_patCon env pc
      | ECon (Option, pc, NONE) => string "NULL"
      | ECon (Option, pc, SOME e) =>
        let
            val to = case pc of
                         PConVar n => #2 (E.lookupConstructor env n)
                       | PConFfi {arg, ...} => arg

            val t = case to of
                        NONE => raise Fail "CjrPrint: ECon argument status mismatch"
                      | SOME t => t
        in
            case #1 t of
                TDatatype _ => p_exp' par env e
              | TFfi ("Basis", "string") => p_exp' par env e
              | _ => box [string "({",
                          newline,
                          p_typ env t,
                          space,
                          string "*tmp",
                          space,
                          string "=",
                          space,
                          string "lw_malloc(ctx, sizeof(",
                          p_typ env t,
                          string "));",
                          newline,
                          string "*tmp",
                          space,
                          string "=",
                          p_exp' par env e,
                          string ";",
                          newline,
                          string "tmp;",
                          newline,
                          string "})"]
        end
      | ECon (Default, pc, eo) =>
        let
            val (xd, xc, xn) = patConInfo env pc
        in
            box [string "({",
                 newline,
                 string "struct",
                 space,
                 string xd,
                 space,
                 string "*tmp",
                 space,
                 string "=",
                 space,
                 string "lw_malloc(ctx, sizeof(struct ",
                 string xd,
                 string "));",
                 newline,
                 string "tmp->tag",
                 space,
                 string "=",
                 space,
                 string xc,
                 string ";",
                 newline,
                 case eo of
                     NONE => box []
                   | SOME e => box [string "tmp->data.",
                                    string xn,
                                    space,
                                    string "=",
                                    space,
                                    p_exp env e,
                                    string ";",
                                    newline],
                 string "tmp;",
                 newline,
                 string "})"]          
        end

      | EFfi (m, x) => box [string "lw_", string m, string "_", string x]
      | EFfiApp (m, x, es) => box [string "lw_",
                                   string m,
                                   string "_",
                                   string x,
                                   string "(ctx, ",
                                   p_list (p_exp env) es,
                                   string ")"]
      | EApp (e1, e2) =>
        let
            fun unravel (f, acc) =
                case #1 f of
                    EApp (f', arg) => unravel (f', arg :: acc)
                  | _ => (f, acc)

            val (f, args) = unravel (e1, [e2])
        in
            parenIf par (box [p_exp' true env e1,
                              string "(ctx,",
                              space,
                              p_list_sep (box [string ",", space]) (p_exp env) args,
                              string ")"])
        end

      | ERecord (i, xes) => box [string "({",
                                 space,
                                 string "struct",
                                 space,
                                 string ("__lws_" ^ Int.toString i),
                                 space,
                                 string "tmp",
                                 space,
                                 string "=",
                                 space,
                                 string "{",
                                 p_list (fn (_, e) =>
                                            p_exp env e) xes,
                                 string "};",
                                 space,
                                 string "tmp;",
                                 space,
                                 string "})" ]
      | EField (e, x) =>
        box [p_exp' true env e,
             string ".__lwf_",
             string x]

      | ECase (e, pes, {disc, result}) =>
        let
            val final = newGoto ()

            val body = foldl (fn ((p, e), body) =>
                               let
                                   val exit = newGoto ()
                                   val (pr, _) = p_pat_preamble env p
                                   val (p, env) = p_pat (env,
                                                         box [string "goto",
                                                              space,
                                                              exit,
                                                              string ";"],
                                                         0) p
                               in
                                   box [body,
                                        box [string "{",
                                             newline,
                                             pr,
                                             newline,
                                             p,
                                             newline,
                                             string "result",
                                             space,
                                             string "=",
                                             space,
                                             p_exp env e,
                                             string ";",
                                             newline,
                                             string "goto",
                                             space,
                                             final,
                                             string ";",
                                             newline,
                                             string "}"],
                                        newline,
                                        exit,
                                        string ":",
                                        newline]
                               end) (box []) pes
        in
            box [string "({",
                 newline,
                 p_typ env disc,
                 space,
                 string "disc0",
                 space,
                 string "=",
                 space,
                 p_exp env e,
                 string ";",
                 newline,
                 p_typ env result,
                 space,
                 string "result;",
                 newline,
                 body,
                 string "lw_error(ctx, FATAL, \"",
                 string (ErrorMsg.spanToString loc),
                 string ": pattern match failure\");",
                 newline,
                 final,
                 string ":",
                 space,
                 string "result;",
                 newline,
                 string "})"]
        end

      | EWrite e => box [string "(lw_write(ctx, ",
                         p_exp env e,
                         string "), lw_unit_v)"]

      | ESeq (e1, e2) => box [string "(",
                              p_exp env e1,
                              string ",",
                              space,
                              p_exp env e2,
                              string ")"]
      | ELet (x, t, e1, e2) => box [string "({",
                                    newline,
                                    p_typ env t,
                                    space,
                                    string "__lwr_",
                                    string x,
                                    string "_",
                                    string (Int.toString (E.countERels env)),
                                    space,
                                    string "=",
                                    space,
                                    p_exp env e1,
                                    string ";",
                                    newline,
                                    p_exp (E.pushERel env x t) e2,
                                    string ";",
                                    newline,
                                    string "})"]

      | EQuery {exps, tables, rnum, state, query, body, initial} =>
        string "(lw_error(ctx, FATAL, \"I would have run a query.\"), NULL)"

and p_exp env = p_exp' false env

fun p_fun env (fx, n, args, ran, e) =
    let
        val nargs = length args
        val env' = foldl (fn ((x, dom), env) => E.pushERel env x dom) env args
    in
        box [string "static",
             space,
             p_typ env ran,
             space,
             string ("__lwn_" ^ fx ^ "_" ^ Int.toString n),
             string "(",
             p_list_sep (box [string ",", space]) (fn x => x)
                        (string "lw_context ctx" :: ListUtil.mapi (fn (i, (_, dom)) =>
                                                                      box [p_typ env dom,
                                                                           space,
                                                                           p_rel env' (nargs - i - 1)]) args),
             string ")",
             space,
             string "{",
             newline,
             box[string "return(",
                 p_exp env' e,
                 string ");"],
             newline,
             string "}"]
    end

fun p_decl env (dAll as (d, _) : decl) =
    case d of
        DStruct (n, xts) =>
        let
            val env = E.declBinds env dAll
        in
            box [string "struct",
                 space,
                 string ("__lws_" ^ Int.toString n),
                 space,
                 string "{",
                 newline,
                 p_list_sep (box []) (fn (x, t) => box [p_typ env t,
                                                        space,
                                                        string "__lwf_",
                                                        string x,
                                                        string ";",
                                                        newline]) xts,
                 string "};"]
        end
      | DDatatype (Enum, x, n, xncs) =>
        box [string "enum",
             space,
             string ("__lwe_" ^ x ^ "_" ^ Int.toString n),
             space,
             string "{",
             space,
             p_list_sep (box [string ",", space]) (fn (x, n, _) => string ("__lwc_" ^ x ^ "_" ^ Int.toString n)) xncs,
             space,
             string "};"]
      | DDatatype (Option, _, _, _) => box []
      | DDatatype (Default, x, n, xncs) =>
        let
            val xncsArgs = List.mapPartial (fn (x, n, NONE) => NONE
                                             | (x, n, SOME t) => SOME (x, n, t)) xncs
        in
            box [string "enum",
                 space,
                 string ("__lwe_" ^ x ^ "_" ^ Int.toString n),
                 space,
                 string "{",
                 space,
                 p_list_sep (box [string ",", space]) (fn (x, n, _) => string ("__lwc_" ^ x ^ "_" ^ Int.toString n)) xncs,
                 space,
                 string "};",
                 newline,
                 newline,
                 string "struct",
                 space,
                 string ("__lwd_" ^ x ^ "_" ^ Int.toString n),
                 space,
                 string "{",
                 newline,
                 string "enum",
                 space,
                 string ("__lwe_" ^ x ^ "_" ^ Int.toString n),
                 space,
                 string "tag;",
                 newline,
                 box (case xncsArgs of
                          [] => []
                        | _ => [string "union",
                                space,
                                string "{",
                                newline,
                                p_list_sep newline (fn (x, n, t) => box [p_typ env t,
                                                                         space,
                                                                         string ("lw_" ^ x),
                                                                         string ";"]) xncsArgs,
                                newline,
                                string "}",
                                space,
                                string "data;",
                                newline]),
                 string "};"]
        end

      | DDatatypeForward _ => box []

      | DVal (x, n, t, e) =>
        box [p_typ env t,
             space,
             string ("__lwn_" ^ x ^ "_" ^ Int.toString n),
             space,
             string "=",
             space,
             p_exp env e,
             string ";"]
      | DFun vi => p_fun env vi
      | DFunRec vis =>
        let
            val env = E.declBinds env dAll
        in
            box [p_list_sep newline (fn (fx, n, args, ran, _) =>
                                        box [string "static",
                                             space,
                                             p_typ env ran,
                                             space,
                                             string ("__lwn_" ^ fx ^ "_" ^ Int.toString n),
                                             string "(lw_context,",
                                             space,
                                             p_list_sep (box [string ",", space])
                                                        (fn (_, dom) => p_typ env dom) args,
                                             string ");"]) vis,
                 newline,
                 p_list_sep newline (p_fun env) vis,
                 newline]
        end
      | DTable (x, _) => box [string "/* SQL table ",
                              string x,
                              string " */",
                              newline]
      | DDatabase s => box [string "void lw_db_init(lw_context ctx) {",
                            newline,
                            string "PGresult *res;",
                            newline,
                            string "PGconn *conn = PQconnectdb(\"",
                            string (String.toString s),
                            string "\");",
                            newline,
                            string "if (conn == NULL) lw_error(ctx, BOUNDED_RETRY, ",
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
                                 string "lw_error(ctx, BOUNDED_RETRY, ",
                                 string "\"Connection to Postgres server failed: %s\", msg);"],
                            newline,
                            string "}",
                            newline,
                            string "lw_set_db(ctx, conn);",
                            newline,
                            string "}",
                            newline,
                            newline,
                            string "void lw_db_close(lw_context ctx) {",
                            newline,
                            string "PQfinish(lw_get_db(ctx));",
                            newline,
                            string "}",
                            newline]

datatype 'a search =
         Found of 'a
       | NotFound
       | Error


fun p_file env (ds, ps) =
    let
        val (pds, env) = ListUtil.foldlMap (fn (d, env) =>
                                             (p_decl env d,
                                              E.declBinds env d))
                             env ds

        val fields = foldl (fn ((ek, _, _, ts), fields) =>
                               case ek of
                                   Core.Link => fields
                                 | Core.Action =>
                                   case List.last ts of
                                       (TRecord i, _) =>
                                       let
                                           val xts = E.lookupStruct env i
                                           val xtsSet = SS.addList (SS.empty, map #1 xts)
                                       in
                                           foldl (fn ((x, _), fields) =>
                                                     let
                                                         val xtsSet' = Option.getOpt (SM.find (fields, x), SS.empty)
                                                     in
                                                         SM.insert (fields, x, SS.union (SS.delete (xtsSet, x),
                                                                                         xtsSet'))
                                                     end) fields xts
                                       end
                                     | _ => raise Fail "CjrPrint: Last argument of action isn't record")
                     SM.empty ps

        val fnums = SM.foldli (fn (x, xs, fnums) =>
                                  let
                                      val unusable = SS.foldl (fn (x', unusable) =>
                                                                  case SM.find (fnums, x') of
                                                                      NONE => unusable
                                                                    | SOME n => IS.add (unusable, n))
                                                     IS.empty xs

                                      fun findAvailable n =
                                          if IS.member (unusable, n) then
                                              findAvailable (n + 1)
                                          else
                                              n
                                  in
                                      SM.insert (fnums, x, findAvailable 0)
                                  end)
                    SM.empty fields

        fun makeSwitch (fnums, i) =
            case SM.foldl (fn (n, NotFound) => Found n
                            | (n, Error) => Error
                            | (n, Found n') => if n = n' then
                                                  Found n'
                                               else
                                                   Error) NotFound fnums of
                NotFound => box [string "return",
                                 space,
                                 string "-1;"]
              | Found n => box [string "return",
                                space,
                                string (Int.toString n),
                                string ";"]
              | Error =>
                let
                    val cmap = SM.foldli (fn (x, n, cmap) =>
                                             let
                                                 val ch = if i < size x then
                                                              String.sub (x, i)
                                                          else
                                                              chr 0

                                                 val fnums = case CM.find (cmap, ch) of
                                                                 NONE => SM.empty
                                                               | SOME fnums => fnums
                                                 val fnums = SM.insert (fnums, x, n)
                                             in
                                                 CM.insert (cmap, ch, fnums)
                                             end)
                               CM.empty fnums

                    val cmap = CM.listItemsi cmap
                in
                    case cmap of
                        [(_, fnums)] =>
                        box [string "if",
                             space,
                             string "(name[",
                             string (Int.toString i),
                             string "]",
                             space,
                             string "==",
                             space,
                             string "0)",
                             space,
                             string "return",
                             space,
                             string "-1;",
                             newline,
                             makeSwitch (fnums, i+1)]
                      | _ =>
                        box [string "switch",
                             space,
                             string "(name[",
                             string (Int.toString i),
                             string "])",
                             space,
                             string "{",
                             newline,
                             box (map (fn (ch, fnums) =>
                                          box [string "case",
                                               space,
                                               if ch = chr 0 then
                                                   string "0:"
                                               else
                                                   box [string "'",
                                                        string (Char.toString ch),
                                                        string "':"],
                                               newline,
                                               makeSwitch (fnums, i+1),
                                               newline]) cmap),
                             string "default:",
                             newline,
                             string "return",
                             space,
                             string "-1;",
                             newline,
                             string "}"]
                end

        fun capitalize s =
            if s = "" then
                ""
            else
                str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

        fun unurlify (t, loc) =
            case t of
                TFfi (m, t) => string ("lw_" ^ m ^ "_unurlify" ^ capitalize t ^ "(ctx, &request)")

              | TRecord 0 => string "lw_unit_v"
              | TRecord i =>
                let
                    val xts = E.lookupStruct env i
                in
                    box [string "({",
                         newline,
                         box (map (fn (x, t) =>
                                      box [p_typ env t,
                                           space,
                                           string x,
                                           space,
                                           string "=",
                                           space,
                                           unurlify t,
                                           string ";",
                                           newline]) xts),
                         string "struct",
                         space,
                         string "__lws_",
                         string (Int.toString i),
                         space,
                         string "tmp",
                         space,
                         string "=",
                         space,
                         string "{",
                         space,
                         p_list_sep (box [string ",", space]) (fn (x, _) => string x) xts,
                         space,
                         string "};",
                         newline,
                         string "tmp;",
                         newline,
                         string "})"]
                end

              | TDatatype (Enum, i, _) =>
                let
                    val (x, xncs) = E.lookupDatatype env i

                    fun doEm xncs =
                        case xncs of
                            [] => string ("(lw_error(ctx, FATAL, \"Error unurlifying datatype " ^ x ^ "\"), (enum __lwe_"
                                          ^ x ^ "_" ^ Int.toString i ^ ")0)")
                          | (x', n, to) :: rest =>
                            box [string "((!strncmp(request, \"",
                                 string x',
                                 string "\", ",
                                 string (Int.toString (size x')),
                                 string ") && (request[",
                                 string (Int.toString (size x')),
                                 string "] == 0 || request[",
                                 string (Int.toString (size x')),
                                 string ("] == '/')) ? __lwc_" ^ x' ^ "_" ^ Int.toString n),
                                 space,
                                 string ":",
                                 space,
                                 doEm rest,
                                 string ")"]
                in
                    doEm xncs
                end

              | TDatatype (Option, i, xncs) =>
                let
                    val (x, _) = E.lookupDatatype env i

                    val (no_arg, has_arg, t) =
                        case !xncs of
                            [(no_arg, _, NONE), (has_arg, _, SOME t)] =>
                            (no_arg, has_arg, t)
                          | [(has_arg, _, SOME t), (no_arg, _, NONE)] =>
                            (no_arg, has_arg, t)
                          | _ => raise Fail "CjrPrint: unfooify misclassified Option datatype"
                in
                    box [string "(request[0] == '/' ? ++request : request,",
                         newline,
                         string "((!strncmp(request, \"",
                         string no_arg,
                         string "\", ",
                         string (Int.toString (size no_arg)),
                         string ") && (request[",
                         string (Int.toString (size no_arg)),
                         string "] == 0 || request[",
                         string (Int.toString (size no_arg)),
                         string "] == '/')) ? (request",
                         space,
                         string "+=",
                         space,
                         string (Int.toString (size no_arg)),
                         string ", NULL) : ((!strncmp(request, \"",
                         string has_arg,
                         string "\", ",
                         string (Int.toString (size has_arg)),
                         string ") && (request[",
                         string (Int.toString (size has_arg)),
                         string "] == 0 || request[",
                         string (Int.toString (size has_arg)),
                         string "] == '/')) ? (request",
                         space,
                         string "+=",
                         space,
                         string (Int.toString (size has_arg)),
                         string ", (request[0] == '/' ? ++request : NULL), ",
                         newline,
                         
                         case #1 t of
                             TDatatype _ => unurlify t
                           | TFfi ("Basis", "string") => unurlify t
                           | _ => box [string "({",
                                       newline,
                                       p_typ env t,
                                       space,
                                       string "*tmp",
                                       space,
                                       string "=",
                                       space,
                                       string "lw_malloc(ctx, sizeof(",
                                       p_typ env t,
                                       string "));",
                                       newline,
                                       string "*tmp",
                                       space,
                                       string "=",
                                       space,
                                       unurlify t,
                                       string ";",
                                       newline,
                                       string "tmp;",
                                       newline,
                                       string "})"],
                         string ")",
                         newline,
                         string ":",
                         space,
                         string ("(lw_error(ctx, FATAL, \"Error unurlifying datatype " ^ x ^ "\"), NULL))))")]
                end                     

              | TDatatype (Default, i, _) =>
                let
                    val (x, xncs) = E.lookupDatatype env i

                    fun doEm xncs =
                        case xncs of
                            [] => string ("(lw_error(ctx, FATAL, \"Error unurlifying datatype " ^ x ^ "\"), NULL)")
                          | (x', n, to) :: rest =>
                            box [string "((!strncmp(request, \"",
                                 string x',
                                 string "\", ",
                                 string (Int.toString (size x')),
                                 string ") && (request[",
                                 string (Int.toString (size x')),
                                 string "] == 0 || request[",
                                 string (Int.toString (size x')),
                                 string "] == '/')) ? ({",
                                 newline,
                                 string "struct",
                                 space,
                                 string ("__lwd_" ^ x ^ "_" ^ Int.toString i),
                                 space,
                                 string "*tmp = lw_malloc(ctx, sizeof(struct __lwd_",
                                 string x,
                                 string "_",
                                 string (Int.toString i),
                                 string "));",
                                 newline,
                                 string "tmp->tag",
                                 space,
                                 string "=",
                                 space,
                                 string ("__lwc_" ^ x' ^ "_" ^ Int.toString n),
                                 string ";",
                                 newline,
                                 string "request",
                                 space,
                                 string "+=",
                                 space,
                                 string (Int.toString (size x')),
                                 string ";",
                                 newline,
                                 string "if (request[0] == '/') ++request;",
                                 newline,
                                 case to of
                                     NONE => box []
                                   | SOME t => box [string "tmp->data.lw_",
                                                    string x',
                                                    space,
                                                    string "=",
                                                    space,
                                                    unurlify t,
                                                    string ";",
                                                    newline],
                                 string "tmp;",
                                 newline,
                                 string "})",
                                 space,
                                 string ":",
                                 space,
                                 doEm rest,
                                 string ")"]
                in
                    doEm xncs
                end

              | _ => (ErrorMsg.errorAt loc "Unable to choose a URL decoding function";
                      space)


        fun p_page (ek, s, n, ts) =
            let
                val (ts, defInputs, inputsVar) =
                    case ek of
                        Core.Link => (ts, string "", string "")
                      | Core.Action =>
                        case List.last ts of
                            (TRecord i, _) =>
                            let
                                val xts = E.lookupStruct env i
                            in
                                (List.drop (ts, 1),
                                 box [box (map (fn (x, t) => box [p_typ env t,
                                                                  space,
                                                                  string "lw_input_",
                                                                  string x,
                                                                  string ";",
                                                                  newline]) xts),
                                      newline,
                                      box (map (fn (x, t) =>
                                                   let
                                                       val n = case SM.find (fnums, x) of
                                                                   NONE => raise Fail "CjrPrint: Can't find in fnums"
                                                                 | SOME n => n

                                                       val f = case t of
                                                                   (TFfi ("Basis", "bool"), _) => "optional_"
                                                                 | _ => ""
                                                   in
                                                       box [string "request = lw_get_",
                                                            string f,
                                                            string "input(ctx, ",
                                                            string (Int.toString n),
                                                            string ");",
                                                            newline,
                                                            string "if (request == NULL) {",
                                                            newline,
                                                            box [string "printf(\"Missing input ",
                                                                 string x,
                                                                 string "\\n\");",
                                                                 newline,
                                                                 string "exit(1);"],
                                                            newline,
                                                            string "}",
                                                            newline,
                                                            string "lw_input_",
                                                            string x,
                                                            space,
                                                            string "=",
                                                            space,
                                                            unurlify t,
                                                            string ";",
                                                            newline]
                                                   end) xts),
                                      string "struct __lws_",
                                      string (Int.toString i),
                                      space,
                                      string "lw_inputs",
                                      space,
                                      string "= {",
                                      newline,
                                      box (map (fn (x, _) => box [string "lw_input_",
                                                                  string x,
                                                                  string ",",
                                                                  newline]) xts),
                                      string "};",
                                      newline],
                                 box [string ",",
                                      space,
                                      string "lw_inputs"])
                            end

                          | _ => raise Fail "CjrPrint: Last argument to an action isn't a record"
            in
                box [string "if (!strncmp(request, \"",
                     string (String.toString s),
                     string "\", ",
                     string (Int.toString (size s)),
                     string ") && (request[",
                     string (Int.toString (size s)),
                     string "] == 0 || request[",
                     string (Int.toString (size s)),
                     string "] == '/')) {",
                     newline,
                     string "request += ",
                     string (Int.toString (size s)),
                     string ";",
                     newline,
                     string "if (*request == '/') ++request;",
                     newline,
                     box [string "{",
                          newline,
                          box (ListUtil.mapi (fn (i, t) => box [p_typ env t,
                                                                space,
                                                                string "arg",
                                                                string (Int.toString i),
                                                                space,
                                                                string "=",
                                                                space,
                                                                unurlify t,
                                                                string ";",
                                                                newline]) ts),
                          defInputs,
                          p_enamed env n,
                          string "(",
                          p_list_sep (box [string ",", space])
                                     (fn x => x)
                                     (string "ctx"
                                      :: ListUtil.mapi (fn (i, _) => string ("arg" ^ Int.toString i)) ts
                                      @ [string "lw_unit_v"]),
                          inputsVar,
                          string ");",
                          newline,
                          string "return;",
                          newline,
                          string "}",
                          newline,
                          string "}"]
                    ]
            end

        val pds' = map p_page ps
    in
        box [string "#include <stdio.h>",
             newline,
             string "#include <stdlib.h>",
             newline,
             string "#include <string.h>",
             newline,
             string "#include <postgresql/libpq-fe.h>",
             newline,
             newline,
             string "#include \"urweb.h\"",
             newline,
             newline,
             p_list_sep newline (fn x => x) pds,
             newline,
             string "int lw_inputs_len = ",
             string (Int.toString (SM.foldl Int.max 0 fnums + 1)),
             string ";",
             newline,
             newline,
             string "int lw_input_num(char *name) {",
             newline,
             makeSwitch (fnums, 0),
             string "}",
             newline,
             newline,
             string "void lw_handle(lw_context ctx, char *request) {",
             newline,
             p_list_sep newline (fn x => x) pds',
             newline,
             string "}",
             newline]
    end

end
