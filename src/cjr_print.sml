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

val ident = String.translate (fn #"'" => "PRIME"
                               | ch => str ch)

val p_ident = string o ident

fun isUnboxable (t : typ) =
    case #1 t of
        TDatatype (Default, _, _) => true
      | TFfi ("Basis", "string") => true
      | TFfi ("Basis", "queryString") => true
      | _ => false

fun p_typ' par env (t, loc) =
    case t of
        TFun (t1, t2) => (EM.errorAt loc "Function type remains";
                          string "<FUNCTION>")
      | TRecord 0 => string "uw_unit"
      | TRecord i => box [string "struct",
                          space,
                          string "__uws_",
                          string (Int.toString i)]
      | TDatatype (Enum, n, _) =>
        (box [string "enum",
              space,
              string ("__uwe_" ^ ident (#1 (E.lookupDatatype env n)) ^ "_" ^ Int.toString n)]
         handle CjrEnv.UnboundNamed _ => string ("__uwd_UNBOUND__" ^ Int.toString n))
      | TDatatype (Option, n, xncs) =>
        (case ListUtil.search #3 (!xncs) of
             NONE => raise Fail "CjrPrint: TDatatype marked Option has no constructor with an argument"
           | SOME t =>
             if isUnboxable t then
                 p_typ' par env t
             else
                 box [p_typ' par env t,
                      string "*"])
      | TDatatype (Default, n, _) =>
        (box [string "struct",
              space,
              string ("__uwd_" ^ ident (#1 (E.lookupDatatype env n)) ^ "_" ^ Int.toString n ^ "*")]
         handle CjrEnv.UnboundNamed _ => string ("__uwd_UNBOUND__" ^ Int.toString n))
      | TFfi (m, x) => box [string "uw_", p_ident m, string "_", p_ident x]
      | TOption t =>
        if isUnboxable t then
            p_typ' par env t
        else
            box [p_typ' par env t,
                 string "*"]
      | TList (_, i) => box [string "struct",
                             space,
                             string "__uws_",
                             string (Int.toString i),
                             string "*"]

and p_typ env = p_typ' false env

fun p_rel env n = string ("__uwr_" ^ ident (#1 (E.lookupERel env n)) ^ "_" ^ Int.toString (E.countERels env - n - 1))
    handle CjrEnv.UnboundRel _ => string ("__uwr_UNBOUND_" ^ Int.toString (E.countERels env - n - 1))

fun p_enamed' env n =
    "__uwn_" ^ ident (#1 (E.lookupENamed env n)) ^ "_" ^ Int.toString n
    handle CjrEnv.UnboundNamed _ => "__uwn_UNBOUND_" ^ Int.toString n

fun p_enamed env n = string (p_enamed' env n)

fun p_con_named env n =
    string ("__uwc_" ^ ident (#1 (E.lookupConstructor env n)) ^ "_" ^ Int.toString n)
    handle CjrEnv.UnboundNamed _ => string ("__uwc_UNBOUND_" ^ Int.toString n)

fun p_pat_preamble env (p, _) =
    case p of
        PWild => (box [],
                  env)
      | PVar (x, t) => (box [p_typ env t,
                             space,
                             string "__uwr_",
                             p_ident x,
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
      | PNone _ => (box [], env)
      | PSome (_, p) => p_pat_preamble env p

fun p_patCon env pc =
    case pc of
        PConVar n => p_con_named env n
      | PConFfi {mod = m, con, ...} => string ("uw_" ^ ident m ^ "_" ^ ident con)

fun p_patMatch (env, disc) (p, loc) =
    case p of
        PWild => string "1"
      | PVar _ => string "1"
      | PPrim (Prim.Int n) => box [string ("(" ^ disc),
                                   space,
                                   string "==",
                                   space,
                                   Prim.p_t_GCC (Prim.Int n),
                                   string ")"]
      | PPrim (Prim.String s) => box [string ("!strcmp(" ^ disc),
                                      string ",",
                                      space,
                                      Prim.p_t_GCC (Prim.String s),
                                      string ")"]
      | PPrim (Prim.Char ch) => box [string ("(" ^ disc),
                                     space,
                                     string "==",
                                     space,
                                     Prim.p_t_GCC (Prim.Char ch),
                                     string ")"]
      | PPrim _ => raise Fail "CjrPrint: Disallowed PPrim primitive"

      | PCon (dk, pc, po) =>
        let
            val p =
                case po of
                    NONE => box []
                  | SOME p =>
                    let
                        val (x, to) = case pc of
                                          PConVar n =>
                                          let
                                              val (x, to, _) = E.lookupConstructor env n
                                          in
                                              ("uw_" ^ ident x, to)
                                          end
                                        | PConFfi {mod = m, con, arg, ...} =>
                                          ("uw_" ^ ident m ^ "_" ^ ident con, arg)

                        val t = case to of
                                    NONE => raise Fail "CjrPrint: Constructor mismatch"
                                  | SOME t => t

                        val x = case pc of
                                    PConVar n =>
                                    let
                                        val (x, _, _) = E.lookupConstructor env n
                                    in
                                        "uw_" ^ ident x
                                    end
                                  | PConFfi {mod = m, con, ...} =>
                                    "uw_" ^ ident m ^ "_" ^ ident con

                        val disc' = case dk of
                                        Enum => raise Fail "CjrPrint: Looking at argument of no-argument constructor"
                                      | Default => disc ^ "->data." ^ x
                                      | Option =>
                                        if isUnboxable t then
                                            disc
                                        else
                                            "(*" ^ disc ^ ")"

                        val p = p_patMatch (env, disc') p
                    in
                        box [space,
                             string "&&",
                             space,
                             p]
                    end
        in
            box [string disc,
                 case (dk, po) of
                     (Enum, _) => box [space,
                                       string "==",
                                       space,
                                       p_patCon env pc]
                   | (Default, _) => box [string "->tag",
                                          space,
                                          string "==",
                                          space,
                                          p_patCon env pc]
                   | (Option, NONE) => box [space,
                                            string "==",
                                            space,
                                            string "NULL"]
                   | (Option, SOME _) => box [space,
                                              string "!=",
                                              space,
                                              string "NULL"],
                 p]
        end

      | PRecord [] => string "1"
      | PRecord xps =>
        p_list_sep (box [space, string "&&", space]) (fn (x, p, _) => p_patMatch (env, disc ^ ".__uwf_" ^ ident x) p) xps

      | PNone _ =>
        box [string disc,
             space,
             string "==",
             space,
             string "NULL"]

      | PSome (t, p) =>
        let
            val disc' = if isUnboxable t then
                            disc
                        else
                            "(*" ^ disc ^ ")"

            val p = p_patMatch (env, disc') p
        in
            box [string disc,
                 space,
                 string "!=",
                 space,
                 string "NULL",
                 space,
                 string "&&",
                 space,
                 p]
        end

fun p_patBind (env, disc) (p, loc) =
    case p of
        PWild =>
        (box [], env)
      | PVar (x, t) =>
        (box [p_typ env t,
              space,
              string "__uwr_",
              p_ident x,
              string "_",
              string (Int.toString (E.countERels env)),
              space,
              string "=",
              space,
              string disc,
              string ";",
              newline],
         E.pushERel env x t)
      | PPrim _ => (box [], env)

      | PCon (_, _, NONE) => (box [], env)

      | PCon (dk, pc, SOME p) =>
        let
            val (x, to) = case pc of
                              PConVar n =>
                              let
                                  val (x, to, _) = E.lookupConstructor env n
                              in
                                  ("uw_" ^ ident x, to)
                              end
                            | PConFfi {mod = m, con, arg, ...} =>
                              ("uw_" ^ ident m ^ "_" ^ ident con, arg)

            val t = case to of
                        NONE => raise Fail "CjrPrint: Constructor mismatch"
                      | SOME t => t

            val disc' = case dk of
                            Enum => raise Fail "CjrPrint: Looking at argument of no-argument constructor"
                          | Default => disc ^  "->data." ^ x
                          | Option =>
                            if isUnboxable t then
                                disc
                            else
                                "(*" ^ disc ^ ")"
        in
            p_patBind (env, disc') p
        end

      | PRecord xps =>
        let
            val (xps, env) =
                ListUtil.foldlMap (fn ((x, p, t), env) => p_patBind (env, disc ^ ".__uwf_" ^ ident x) p)
                                  env xps
        in
            (p_list_sep (box []) (fn x => x) xps,
             env)
        end

      | PNone _ => (box [], env)

      | PSome (t, p) =>
        let
            val disc' = if isUnboxable t then
                            disc
                        else
                            "(*" ^ disc ^ ")"
        in
            p_patBind (env, disc') p
        end

fun patConInfo env pc =
    case pc of
        PConVar n =>
        let
            val (x, _, dn) = E.lookupConstructor env n
            val (dx, _) = E.lookupDatatype env dn
        in
            ("__uwd_" ^ ident dx ^ "_" ^ Int.toString dn,
             "__uwc_" ^ ident x ^ "_" ^ Int.toString n,
             "uw_" ^ ident x)
        end
      | PConFfi {mod = m, datatyp, con, ...} =>
        ("uw_" ^ ident m ^ "_" ^ ident datatyp,
         "uw_" ^ ident m ^ "_" ^ ident con,
         "uw_" ^ ident con)

fun p_unsql wontLeakStrings env (tAll as (t, loc)) e eLen =
    case t of
        TFfi ("Basis", "int") => box [string "uw_Basis_stringToInt_error(ctx, ", e, string ")"]
      | TFfi ("Basis", "float") => box [string "uw_Basis_stringToFloat_error(ctx, ", e, string ")"]
      | TFfi ("Basis", "string") =>
        if wontLeakStrings then
            e
        else
            box [string "uw_strdup(ctx, ", e, string ")"]
      | TFfi ("Basis", "bool") => box [string "uw_Basis_stringToBool_error(ctx, ", e, string ")"]
      | TFfi ("Basis", "time") => box [string "uw_Basis_stringToTime_error(ctx, ", e, string ")"]
      | TFfi ("Basis", "blob") => box [string "uw_Basis_stringToBlob_error(ctx, ",
                                       e,
                                       string ", ",
                                       eLen,
                                       string ")"]
      | TFfi ("Basis", "channel") => box [string "uw_Basis_stringToChannel_error(ctx, ", e, string ")"]
      | TFfi ("Basis", "client") => box [string "uw_Basis_stringToClient_error(ctx, ", e, string ")"]

      | _ => (ErrorMsg.errorAt loc "Don't know how to unmarshal type from SQL";
              Print.eprefaces' [("Type", p_typ env tAll)];
              string "ERROR")

fun p_getcol wontLeakStrings env (tAll as (t, loc)) i =
    case t of
        TOption t =>
        box [string "(PQgetisnull(res, i, ",
             string (Int.toString i),
             string ") ? NULL : ",
             case t of
                 (TFfi ("Basis", "string"), _) => p_getcol wontLeakStrings env t i
               | _ => box [string "({",
                           newline,
                           p_typ env t,
                           space,
                           string "*tmp = uw_malloc(ctx, sizeof(",
                           p_typ env t,
                           string "));",
                           newline,
                           string "*tmp = ",
                           p_getcol wontLeakStrings env t i,
                           string ";",
                           newline,
                           string "tmp;",
                           newline,
                           string "})"],
             string ")"]
      | _ =>
        box [string "(PQgetisnull(res, i, ",
             string (Int.toString i),
             string ") ? ",
             box [string "({",
                  p_typ env tAll,
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
             p_unsql wontLeakStrings env tAll
                     (box [string "PQgetvalue(res, i, ",
                           string (Int.toString i),
                           string ")"])
                     (box [string "PQgetlength(res, i, ",
                           string (Int.toString i),
                           string ")"]),
             string ")"]

datatype sql_type = datatype Settings.sql_type
val isBlob = Settings.isBlob

fun isFile (t : typ) =
    case #1 t of
        TFfi ("Basis", "file") => true
      | _ => false

fun p_sql_type t = string (Settings.p_sql_ctype t)

fun getPargs (e, _) =
    case e of
        EPrim (Prim.String _) => []
      | EFfiApp ("Basis", "strcat", [e1, e2]) => getPargs e1 @ getPargs e2

      | EFfiApp ("Basis", "sqlifyInt", [e]) => [(e, Int)]
      | EFfiApp ("Basis", "sqlifyFloat", [e]) => [(e, Float)]
      | EFfiApp ("Basis", "sqlifyString", [e]) => [(e, String)]
      | EFfiApp ("Basis", "sqlifyBool", [e]) => [(e, Bool)]
      | EFfiApp ("Basis", "sqlifyTime", [e]) => [(e, Time)]
      | EFfiApp ("Basis", "sqlifyBlob", [e]) => [(e, Blob)]
      | EFfiApp ("Basis", "sqlifyChannel", [e]) => [(e, Channel)]
      | EFfiApp ("Basis", "sqlifyClient", [e]) => [(e, Client)]

      | ECase (e,
               [((PNone _, _),
                 (EPrim (Prim.String "NULL"), _)),
                ((PSome (_, (PVar _, _)), _),
                 (EFfiApp (m, x, [(ERel 0, _)]), _))],
               _) => map (fn (x, y) => (x, Nullable y)) (getPargs (EFfiApp (m, x, [e]), #2 e))

      | ECase (e,
               [((PCon (_, PConFfi {mod = "Basis", con = "True", ...}, _), _),
                 (EPrim (Prim.String "TRUE"), _)),
                ((PCon (_, PConFfi {mod = "Basis", con = "False", ...}, _), _),
                 (EPrim (Prim.String "FALSE"), _))],
               _) => [(e, Bool)]

      | _ => raise Fail "CjrPrint: getPargs"

val notLeakies = SS.fromList ["int", "float", "char", "time", "bool", "unit", "client", "channel",
                              "xhtml", "page", "xbody", "css_class"]
val notLeakies' = SS.fromList ["blob"]

fun notLeaky env allowHeapAllocated =
    let
        fun nl ok (t, _) =
            case t of
                TFun _ => false
              | TRecord n =>
                let
                    val xts = E.lookupStruct env n
                in
                    List.all (fn (_, t) => nl ok t) xts
                end
              | TDatatype (dk, n, ref cons) =>
                IS.member (ok, n)
                orelse
                ((allowHeapAllocated orelse dk = Enum)
                 andalso
                 let
                     val ok' = IS.add (ok, n)
                 in
                     List.all (fn (_, _, to) => case to of
                                                    NONE => true
                                                  | SOME t => nl ok' t) cons
                 end)
              | TFfi ("Basis", t) => SS.member (notLeakies, t)
                                     orelse (allowHeapAllocated andalso SS.member (notLeakies', t))
              | TFfi _ => false
              | TOption t => allowHeapAllocated andalso nl ok t
              | TList (t, _) => allowHeapAllocated andalso nl ok t
    in
        nl IS.empty
    end

fun capitalize s =
    if s = "" then
        ""
    else
        str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

fun unurlify fromClient env (t, loc) =
    let
        fun unurlify' rf t =
            case t of
                TFfi ("Basis", "unit") => string "uw_Basis_unurlifyUnit(ctx, &request)"
              | TFfi ("Basis", "string") => string (if fromClient then
                                                        "uw_Basis_unurlifyString_fromClient(ctx, &request)"
                                                    else
                                                        "uw_Basis_unurlifyString(ctx, &request)")
              | TFfi (m, t) => string ("uw_" ^ ident m ^ "_unurlify" ^ capitalize t ^ "(ctx, &request)")

              | TRecord 0 => string "uw_Basis_unurlifyUnit(ctx, &request)"
              | TRecord i =>
                let
                    val xts = E.lookupStruct env i
                in
                    box [string "({",
                         newline,
                         box (map (fn (x, t) =>
                                      box [p_typ env t,
                                           space,
                                           string "uwr_",
                                           string x,
                                           space,
                                           string "=",
                                           space,
                                           unurlify' rf (#1 t),
                                           string ";",
                                           newline]) xts),
                         string "struct",
                         space,
                         string "__uws_",
                         string (Int.toString i),
                         space,
                         string "tmp",
                         space,
                         string "=",
                         space,
                         string "{",
                         space,
                         p_list_sep (box [string ",", space]) (fn (x, _) => box [string "uwr_",
                                                                                 string x]) xts,
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
                            [] => string ("(uw_error(ctx, FATAL, \"Error unurlifying datatype "
                                          ^ x ^ "\"), (enum __uwe_"
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
                                 string "] == '/')) ? (request += ",
                                 string (Int.toString (size x')),
                                 string (", (*request == '/' ? ++request : NULL), __uwc_" ^ ident x' ^ "_" ^ Int.toString n ^ ")"),
                                 space,
                                 string ":",
                                 space,
                                 doEm rest,
                                 string ")"]
                in
                    doEm xncs
                end

              | TDatatype (Option, i, xncs) =>
                if IS.member (rf, i) then
                    box [string "unurlify_",
                         string (Int.toString i),
                         string "()"]
                else
                    let
                        val (x, _) = E.lookupDatatype env i

                        val (no_arg, has_arg, t) =
                            case !xncs of
                                [(no_arg, _, NONE), (has_arg, _, SOME t)] =>
                                (no_arg, has_arg, t)
                              | [(has_arg, _, SOME t), (no_arg, _, NONE)] =>
                                (no_arg, has_arg, t)
                              | _ => raise Fail "CjrPrint: unfooify misclassified Option datatype"

                        val rf = IS.add (rf, i)
                    in
                        box [string "({",
                             space,
                             p_typ env t,
                             space,
                             string "*unurlify_",
                             string (Int.toString i),
                             string "(void) {",
                             newline,
                             box [string "return (request[0] == '/' ? ++request : request,",
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
                                  
                                  if isUnboxable  t then
                                      unurlify' rf (#1 t)
                                  else
                                      box [string "({",
                                           newline,
                                           p_typ env t,
                                           space,
                                           string "*tmp",
                                           space,
                                           string "=",
                                           space,
                                           string "uw_malloc(ctx, sizeof(",
                                           p_typ env t,
                                           string "));",
                                           newline,
                                           string "*tmp",
                                           space,
                                           string "=",
                                           space,
                                           unurlify' rf (#1 t),
                                           string ";",
                                           newline,
                                           string "tmp;",
                                           newline,
                                           string "})"],
                                  string ")",
                                  newline,
                                  string ":",
                                  space,
                                  string ("(uw_error(ctx, FATAL, \"Error unurlifying datatype " ^ x
                                          ^ "\"), NULL))));"),
                                  newline],
                             string "}",
                             newline,
                             newline,

                             string "unurlify_",
                             string (Int.toString i),
                             string "();",
                             newline,
                             string "})"]
                    end

              | TDatatype (Default, i, _) =>
                if IS.member (rf, i) then
                    box [string "unurlify_",
                         string (Int.toString i),
                         string "()"]
                else
                    let
                        val (x, xncs) = E.lookupDatatype env i

                        val rf = IS.add (rf, i)

                        fun doEm xncs =
                            case xncs of
                                [] => string ("(uw_error(ctx, FATAL, \"Error unurlifying datatype "
                                              ^ x ^ "\"), NULL)")
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
                                     string ("__uwd_" ^ ident x ^ "_" ^ Int.toString i),
                                     space,
                                     string "*tmp = uw_malloc(ctx, sizeof(struct __uwd_",
                                     string x,
                                     string "_",
                                     string (Int.toString i),
                                     string "));",
                                     newline,
                                     string "tmp->tag",
                                     space,
                                     string "=",
                                     space,
                                     string ("__uwc_" ^ ident x' ^ "_" ^ Int.toString n),
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
                                       | SOME (t, _) => box [string "tmp->data.uw_",
                                                             p_ident x',
                                                             space,
                                                             string "=",
                                                             space,
                                                             unurlify' rf t,
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
                        box [string "({",
                             space,
                             p_typ env (t, ErrorMsg.dummySpan),
                             space,
                             string "unurlify_",
                             string (Int.toString i),
                             string "(void) {",
                             newline,
                             box [string "return",
                                  space,
                                  doEm xncs,
                                  string ";",
                                  newline],
                             string "}",
                             newline,
                             newline,

                             string "unurlify_",
                             string (Int.toString i),
                             string "();",
                             newline,
                             string "})"]
                    end

              | TList (t', i) =>
                if IS.member (rf, i) then
                    box [string "unurlify_list_",
                         string (Int.toString i),
                         string "()"]
                else
                    let
                        val rf = IS.add (rf, i)
                    in
                        box [string "({",
                             space,
                             p_typ env (t, loc),
                             space,
                             string "unurlify_list_",
                             string (Int.toString i),
                             string "(void) {",
                             newline,
                             box [string "return (request[0] == '/' ? ++request : request,",
                                  newline,
                                  string "((!strncmp(request, \"Nil\", 3) && (request[3] == 0 ",
                                  string "|| request[3] == '/')) ? (request",
                                  space,
                                  string "+=",
                                  space,
                                  string "3, (*request == '/' ? *request++ = 0 : 0), NULL) : ((!strncmp(request, \"Cons\", 4) && (request[4] == 0 ",
                                  string "|| request[4] == '/')) ? (request",
                                  space,
                                  string "+=",
                                  space,
                                  string "4, (request[0] == '/' ? ++request : NULL), ",
                                  newline,
                                  
                                  string "({",
                                  newline,
                                  p_typ env (t, loc),
                                  space,
                                  string "tmp",
                                  space,
                                  string "=",
                                  space,
                                  string "uw_malloc(ctx, sizeof(struct __uws_",
                                  string (Int.toString i),
                                  string "));",
                                  newline,
                                  string "*tmp",
                                  space,
                                  string "=",
                                  space,
                                  unurlify' rf (TRecord i),
                                  string ";",
                                  newline,
                                  string "tmp;",
                                  newline,
                                  string "})",
                                  string ")",
                                  newline,
                                  string ":",
                                  space,
                                  string ("(uw_error(ctx, FATAL, \"Error unurlifying list: %s\", request), NULL))));"),
                                  newline],
                             string "}",
                             newline,
                             newline,

                             string "unurlify_list_",
                             string (Int.toString i),
                             string "();",
                             newline,
                             string "})"]
                    end

              | TOption t =>
                box [string "(request[0] == '/' ? ++request : request, ",
                     string "((!strncmp(request, \"None\", 4) ",
                     string "&& (request[4] == 0 || request[4] == '/')) ",
                     string "? (request += (request[4] == 0 ? 4 : 5), NULL) ",
                     string ": ((!strncmp(request, \"Some\", 4) ",
                     string "&& request[4] == '/') ",
                     string "? (request += 5, ",
                     if isUnboxable  t then
                         unurlify' rf (#1 t)
                     else
                         box [string "({",
                              newline,
                              p_typ env t,
                              space,
                              string "*tmp",
                              space,
                              string "=",
                              space,
                              string "uw_malloc(ctx, sizeof(",
                              p_typ env t,
                              string "));",
                              newline,
                              string "*tmp",
                              space,
                              string "=",
                              space,
                              unurlify' rf (#1 t),
                              string ";",
                              newline,
                              string "tmp;",
                              newline,
                              string "})"],
                     string ") :",
                     space,
                     string "(uw_error(ctx, FATAL, \"Error unurlifying option type\"), NULL))))"]

              | _ => (ErrorMsg.errorAt loc "Unable to choose a URL decoding function";
                      space)
    in
        unurlify' IS.empty t
    end

val urlify1 = ref 0

fun urlify env t =
    let
        fun urlify' rf rfl level (t as (_, loc)) =
            case #1 t of
                TFfi ("Basis", "unit") => box []
              | TFfi (m, t) => box [string ("uw_" ^ ident m ^ "_urlify" ^ capitalize t
                                            ^ "_w(ctx, it" ^ Int.toString level ^ ");"),
                                    newline]

              | TRecord 0 => box []
              | TRecord i =>
                let
                    fun empty (t, _) =
                        case t of
                            TFfi ("Basis", "unit") => true
                          | TRecord 0 => true
                          | TRecord j =>
                            List.all (fn (_, t) => empty t) (E.lookupStruct env j)
                          | _ => false

                    val xts = E.lookupStruct env i

                    val (blocks, _) = foldl
                                      (fn ((x, t), (blocks, printingSinceLastSlash)) =>
                                          let
                                              val thisEmpty = empty t
                                          in
                                              if thisEmpty then
                                                  (blocks, printingSinceLastSlash)
                                              else
                                                  (box [string "{",
                                                        newline,
                                                        p_typ env t,
                                                        space,
                                                        string ("it" ^ Int.toString (level + 1)),
                                                        space,
                                                        string "=",
                                                        space,
                                                        string ("it" ^ Int.toString level ^ ".__uwf_" ^ x ^ ";"),
                                                        newline,
                                                        box (if printingSinceLastSlash then
                                                                 [string "uw_write(ctx, \"/\");",
                                                                  newline]
                                                             else
                                                                 []),
                                                        urlify' rf rfl (level + 1) t,
                                                        string "}",
                                                        newline] :: blocks,
                                                   true)
                                          end)
                                      ([], false) xts
                in
                    box (rev blocks)
                end

              | TDatatype (Enum, i, _) =>
                let
                    val (x, xncs) = E.lookupDatatype env i

                    fun doEm xncs =
                        case xncs of
                            [] => box [string ("uw_error(ctx, FATAL, \"Error urlifying datatype "
                                               ^ x ^ "\");"),
                                       newline]
                          | (x', n, to) :: rest =>
                            box [string ("if (it" ^ Int.toString level
                                         ^ "==__uwc_" ^ ident x' ^ "_" ^ Int.toString n ^ ") {"),
                                 newline,
                                 box [string ("uw_write(ctx, \"" ^ x' ^ "\");"),
                                      newline],
                                 string "} else {",
                                 newline,
                                 box [doEm rest,
                                      newline],
                                 string "}"]
                in
                    doEm xncs
                end

              | TDatatype (Option, i, xncs) =>
                if IS.member (rf, i) then
                    box [string "urlify_",
                         string (Int.toString i),
                         string "(it",
                         string (Int.toString level),
                         string ");",
                         newline]
                else
                    let
                        val (x, _) = E.lookupDatatype env i

                        val (no_arg, has_arg, t) =
                            case !xncs of
                                [(no_arg, _, NONE), (has_arg, _, SOME t)] =>
                                (no_arg, has_arg, t)
                              | [(has_arg, _, SOME t), (no_arg, _, NONE)] =>
                                (no_arg, has_arg, t)
                              | _ => raise Fail "CjrPrint: urlify misclassified Option datatype"

                        val rf = IS.add (rf, i)
                    in
                        box [string "({",
                             space,
                             string "void",
                             space,
                             string "urlify_",
                             string (Int.toString i),
                             string "(",
                             p_typ env t,
                             space,
                             if isUnboxable t then
                                 box []
                             else
                                 string "*",
                             string "it0) {",
                             newline,
                             box [string "if (it0) {",
                                  newline,
                                  if isUnboxable t then
                                      urlify' rf rfl 0 t
                                  else
                                      box [p_typ env t,
                                           space,
                                           string "it1",
                                           space,
                                           string "=",
                                           space,
                                           string "*it0;",
                                           newline,
                                           string "uw_write(ctx, \"",
                                           string has_arg,
                                           string "/\");",
                                           newline,
                                           urlify' rf rfl 1 t,
                                           string ";",
                                           newline],
                                  string "} else {",
                                  box [newline,
                                       string "uw_write(ctx, \"",
                                       string no_arg,
                                       string "\");",
                                       newline],
                                  string "}",
                                  newline],
                             string "}",
                             newline,
                             newline,

                             string "urlify_",
                             string (Int.toString i),
                             string "(it",
                             string (Int.toString level),
                             string ");",
                             newline,
                             string "});",
                             newline]
                    end

              | TDatatype (Default, i, _) =>
                if IS.member (rf, i) then
                    box [string "urlify_",
                         string (Int.toString i),
                         string "(it",
                         string (Int.toString level), 
                         string ");",
                         newline]
                else
                    let
                        val (x, xncs) = E.lookupDatatype env i

                        val rf = IS.add (rf, i)

                        fun doEm xncs =
                            case xncs of
                                [] => box [string ("uw_error(ctx, FATAL, \"Error urlifying datatype "
                                                   ^ x ^ " (%d)\", it0->data);"),
                                           newline]
                              | (x', n, to) :: rest =>
                                box [string "if",
                                     space,
                                     string "(it0->tag==__uwc_",
                                     string (ident x'),
                                     string "_",
                                     string (Int.toString n),
                                     string ") {",
                                     newline,
                                     case to of
                                         NONE => box [string "uw_write(ctx, \"",
                                                      string x',
                                                      string "\");",
                                                      newline]
                                       | SOME t => box [string "uw_write(ctx, \"",
                                                        string x',
                                                        string "/\");",
                                                        newline,
                                                        p_typ env t,
                                                        space,
                                                        string "it1",
                                                        space,
                                                        string "=",
                                                        space,
                                                        string "it0->data.uw_",
                                                        string x',
                                                        string ";",
                                                        newline,
                                                        urlify' rf rfl 1 t,
                                                        newline],
                                     string "} else {",
                                     newline,
                                     box [doEm rest,
                                          newline],
                                     string "}",
                                     newline]
                    in
                        box [string "({",
                             space,
                             string "void",
                             space,
                             string "urlify_",
                             string (Int.toString i),
                             string "(",
                             p_typ env t,
                             space,
                             string "it0) {",
                             newline,
                             box [doEm xncs,
                                  newline],
                             newline,
                             string "}",
                             newline,

                             string "urlify_",
                             string (Int.toString i),
                             string "(it",
                             string (Int.toString level),
                             string ");",
                             newline,
                             string "});",
                             newline]
                    end

              | TOption t =>
                box [string "if (it",
                     string (Int.toString level),
                     string ") {",
                     if isUnboxable t then
                         box [string "uw_write(ctx, \"Some/\");",
                              newline,
                              urlify' rf rfl level t]
                     else
                         box [p_typ env t,
                              space,
                              string "it",
                              string (Int.toString (level + 1)),
                              space,
                              string "=",
                              space,
                              string "*it",
                              string (Int.toString level),
                              string ";",
                              newline,
                              string "uw_write(ctx, \"Some/\");",
                              newline,
                              urlify' rf rfl (level + 1) t,
                              string ";",
                              newline],
                     string "} else {",
                     box [newline,
                          string "uw_write(ctx, \"None\");",
                          newline],
                     string "}",
                     newline]

              | TList (t, i) =>
                if IS.member (rfl, i) then
                    box [string "urlifyl_",
                         string (Int.toString i),
                         string "(it",
                         string (Int.toString level),
                         string ");",
                         newline]
                else
                    let
                        val rfl = IS.add (rfl, i)
                    in
                        box [string "({",
                             space,
                             string "void",
                             space,
                             string "urlifyl_",
                             string (Int.toString i),
                             string "(struct __uws_",
                             string (Int.toString i),
                             space,
                             string "*it0) {",
                             newline,
                             box [string "if (it0) {",
                                  newline,
                                  p_typ env t,
                                  space,
                                  string "it1",
                                  space,
                                  string "=",
                                  space,
                                  string "it0->__uwf_1;",
                                  newline,
                                  string "uw_write(ctx, \"Cons/\");",
                                  newline,
                                  urlify' rf rfl 1 t,
                                  string ";",
                                  newline,
                                  string "uw_write(ctx, \"/\");",
                                  newline,
                                  string "urlifyl_",
                                  string (Int.toString i),
                                  string "(it0->__uwf_2);",
                                  newline,
                                  string "} else {",
                                  newline,
                                  box [string "uw_write(ctx, \"Nil\");",
                                       newline],
                                  string "}",
                                  newline],
                             string "}",
                             newline,
                             newline,

                             string "urlifyl_",
                             string (Int.toString i),
                             string "(it",
                             string (Int.toString level),
                             string ");",
                             newline,
                             string "});",
                             newline]
                    end

              | _ => (ErrorMsg.errorAt loc "Unable to choose a URL encoding function";
                      space)
    in
        urlify' IS.empty IS.empty 0 t
    end

fun sql_type_in env (tAll as (t, loc)) =
    case t of
        TFfi ("Basis", "int") => Int
      | TFfi ("Basis", "float") => Float
      | TFfi ("Basis", "string") => String
      | TFfi ("Basis", "char") => Char
      | TFfi ("Basis", "bool") => Bool
      | TFfi ("Basis", "time") => Time
      | TFfi ("Basis", "blob") => Blob
      | TFfi ("Basis", "channel") => Channel
      | TFfi ("Basis", "client") => Client
      | TOption t' => Nullable (sql_type_in env t')
      | _ => (ErrorMsg.errorAt loc "Don't know SQL equivalent of type";
              Print.eprefaces' [("Type", p_typ env tAll)];
              Int)

fun potentiallyFancy (e, _) =
    case e of
        EPrim _ => false
      | ERel _ => false
      | ENamed _ => false
      | ECon (_, _, NONE) => false
      | ECon (_, _, SOME e) => potentiallyFancy e
      | ENone _ => false
      | ESome (_, e) => potentiallyFancy e
      | EFfi _ => false
      | EFfiApp _ => true
      | EApp _ => true
      | EUnop (_, e) => potentiallyFancy e
      | EBinop (_, e1, e2) => potentiallyFancy e1 orelse potentiallyFancy e2
      | ERecord (_, xes) => List.exists (potentiallyFancy o #2) xes
      | EField (e, _) => potentiallyFancy e
      | ECase (e, pes, _) => potentiallyFancy e orelse List.exists (potentiallyFancy o #2) pes
      | EError _ => false
      | EReturnBlob _ => false
      | ERedirect _ => false
      | EWrite e => potentiallyFancy e
      | ESeq (e1, e2) => potentiallyFancy e1 orelse potentiallyFancy e2
      | ELet (_, _, e1, e2) => potentiallyFancy e1 orelse potentiallyFancy e2
      | EQuery _ => true
      | EDml {dml = e, ...} => potentiallyFancy e
      | ENextval {seq = e, ...} => potentiallyFancy e
      | ESetval {seq = e1, count = e2} => potentiallyFancy e1 orelse potentiallyFancy e2
      | EUnurlify _ => true

fun p_exp' par env (e, loc) =
    case e of
        EPrim p => Prim.p_t_GCC p
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
            if isUnboxable t then
                p_exp' par env e
            else
                box [string "({",
                     newline,
                     p_typ env t,
                     space,
                     string "*tmp",
                     space,
                     string "=",
                     space,
                     string "uw_malloc(ctx, sizeof(",
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
                 string "uw_malloc(ctx, sizeof(struct ",
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
      | ENone _ => string "NULL"
      | ESome (t, e) =>
        if isUnboxable t then
            p_exp' par env e
        else
            box [string "({",
                 newline,
                 p_typ env t,
                 space,
                 string "*tmp",
                 space,
                 string "=",
                 space,
                 string "uw_malloc(ctx, sizeof(",
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

      | EFfi (m, x) => box [string "uw_", p_ident m, string "_", p_ident x]
      | EError (e, t) =>
        box [string "({",
             newline,
             p_typ env t,
             space,
             string "tmp;",
             newline,
             string "uw_error(ctx, FATAL, \"",
             string (ErrorMsg.spanToString loc),
             string ": %s\", ",
             p_exp env e,
             string ");",
             newline,
             string "tmp;",
             newline,
             string "})"]
      | EReturnBlob {blob, mimeType, t} =>
        box [string "({",
             newline,
             p_typ env t,
             space,
             string "tmp;",
             newline,
             string "uw_return_blob(ctx, ",
             p_exp env blob,
             string ", ",
             p_exp env mimeType,
             string ");",
             newline,
             string "tmp;",
             newline,
             string "})"]
      | ERedirect (e, t) =>
        box [string "({",
             newline,
             p_typ env t,
             space,
             string "tmp;",
             newline,
             string "uw_redirect(ctx, ",
             p_exp env e,
             string ");",
             newline,
             string "tmp;",
             newline,
             string "})"]
      | EApp ((EError (e, (TFun (_, ran), _)), loc), _) =>
        p_exp env (EError (e, ran), loc)
      | EApp ((EReturnBlob {blob, mimeType, t = (TFun (_, ran), _)}, loc), _) =>
        p_exp env (EReturnBlob {blob = blob, mimeType = mimeType, t = ran}, loc)

      | EFfiApp ("Basis", "strcat", [e1, e2]) =>
        let
            fun flatten e =
                case #1 e of
                    EFfiApp ("Basis", "strcat", [e1, e2]) => flatten e1 @ flatten e2
                  | _ => [e]
        in
            case flatten e1 @ flatten e2 of
                [e1, e2] => box [string "uw_Basis_strcat(ctx, ",
                                 p_exp env e1,
                                 string ",",
                                 p_exp env e2,
                                 string ")"]
              | es => box [string "uw_Basis_mstrcat(ctx, ",
                           p_list (p_exp env) es,
                           string ", NULL)"]
        end

      | EFfiApp (m, x, []) => box [string "uw_",
                                   p_ident m,
                                   string "_",
                                   p_ident x,
                                   string "(ctx)"]

      | EFfiApp (m, x, es) => box [string "uw_",
                                   p_ident m,
                                   string "_",
                                   p_ident x,
                                   string "(ctx, ",
                                   p_list (p_exp env) es,
                                   string ")"]
      | EApp (f, args) =>
        parenIf par (box [p_exp' true env f,
                          string "(ctx,",
                          space,
                          p_list_sep (box [string ",", space]) (p_exp env) args,
                          string ")"])

      | EUnop (s, e1) =>
        parenIf par (box [string s,
                          space,
                          p_exp' true env e1])

      | EBinop (s, e1, e2) =>
        if Char.isAlpha (String.sub (s, size s - 1)) then
            box [string s,
                 string "(",
                 p_exp env e1,
                 string ",",
                 space,
                 p_exp env e2,
                 string ")"]
        else
            parenIf par (box [p_exp' true env e1,
                              space,
                              string s,
                              space,
                              p_exp' true env e2])

      | ERecord (0, _) => string "0"

      | ERecord (i, xes) => box [string "({",
                                 space,
                                 string "struct",
                                 space,
                                 string ("__uws_" ^ Int.toString i),
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
             string ".__uwf_",
             p_ident x]

      | ECase (e, pes, {disc, result}) =>
        box [string "({",
             newline,
             p_typ env disc,
             space,
             string "disc",
             space,
             string "=",
             space,
             p_exp env e,
             string ";",
             newline,
             newline,
             foldr (fn ((p, e), body) =>
                       let
                           val pm = p_patMatch (env, "disc") p
                           val (pb, env) = p_patBind (env, "disc") p
                       in
                           box [pm,
                                space,
                                string "?",
                                space,
                                box [string "({",
                                     pb,
                                     p_exp env e,
                                     string ";",
                                     newline,
                                     string "})"],
                                newline,
                                space,
                                string ":",
                                space,
                                body]
                       end) (box [string "({",
                                  newline,
                                  p_typ env result,
                                  space,
                                  string "tmp;",
                                  newline,
                                  string "uw_error(ctx, FATAL, \"",
                                  string (ErrorMsg.spanToString loc),
                                  string ": pattern match failure\");",
                                  newline,
                                  string "tmp;",
                                  newline,
                                  string "})"]) pes,
             string ";",
             newline,
             string "})"]

      | EWrite e => box [string "(uw_write(ctx, ",
                         p_exp env e,
                         string "), 0)"]

      | ESeq (e1, e2) =>
        let
            val useRegion = potentiallyFancy e1
        in
            box [string "(",
                 if useRegion then
                     box [string "uw_begin_region(ctx),",
                          space]
                 else
                     box [],
                 p_exp env e1,
                 string ",",
                 space,
                 if useRegion then
                     box [string "uw_end_region(ctx),",
                          space]
                 else
                     box [],
                 p_exp env e2,
                 string ")"]
        end
      | ELet (x, t, e1, e2) =>
        let
            val useRegion = notLeaky env false t andalso potentiallyFancy e1
        in
            box [string "({",
                 newline,
                 p_typ env t,
                 space,
                 string "__uwr_",
                 p_ident x,
                 string "_",
                 string (Int.toString (E.countERels env)),
                 space,
                 string "=",
                 space,
                 if useRegion then
                     box [string "(uw_begin_region(ctx),",
                          space]
                 else
                     box [],
                 p_exp env e1,
                 if useRegion then
                     string ")"
                 else
                     box [],
                 string ";",
                 newline,
                 if useRegion then
                     box [string "uw_end_region(ctx);",
                          newline]
                 else
                     box [],
                 p_exp (E.pushERel env x t) e2,
                 string ";",
                 newline,
                 string "})"]
        end

      | EQuery {exps, tables, rnum, state, query, body, initial, prepared} =>
        let
            val exps = map (fn (x, t) => ("__uwf_" ^ ident x, t)) exps
            val tables = ListUtil.mapConcat (fn (x, xts) =>
                                                map (fn (x', t) => ("__uwf_" ^ ident x ^ ".__uwf_" ^ ident x', t)) xts)
                                            tables

            val sort = ListMergeSort.sort (fn ((s1, _), (s2, _)) => String.compare (s1, s2) = GREATER)
            val outputs = sort exps @ sort tables

            val wontLeakStrings = notLeaky env true state
            val wontLeakAnything = notLeaky env false state

            val inputs =
                case prepared of
                    NONE => []
                  | SOME _ => getPargs query

            fun doCols p_getcol =
                box [string "struct __uws_",
                     string (Int.toString rnum),
                     string " __uwr_r_",
                     string (Int.toString (E.countERels env)),
                     string ";",
                     newline,
                     p_typ env state,
                     space,
                     string "__uwr_acc_",
                     string (Int.toString (E.countERels env + 1)),
                     space,
                     string "=",
                     space,
                     string "acc;",
                     newline,
                     newline,

                     if Settings.getDeadlines () then
                         box [string "uw_check_deadline(ctx);",
                              newline]
                     else
                         box [],

                     p_list_sepi (box []) (fn i =>
                                           fn (proj, t) =>
                                              box [string "__uwr_r_",
                                                   string (Int.toString (E.countERels env)),
                                                   string ".",
                                                   string proj,
                                                   space,
                                                   string "=",
                                                   space,
                                                   p_getcol {loc = loc,
                                                             wontLeakStrings = wontLeakStrings,
                                                             col = i,
                                                             typ = sql_type_in env t},
                                                   string ";",
                                                   newline]) outputs,
                     newline,
                     newline,

                     string "acc",
                     space,
                     string "=",
                     space,
                     p_exp (E.pushERel
                                (E.pushERel env "r" (TRecord rnum, loc))
                                "acc" state) 
                           body,
                     string ";",
                     newline]
        in
            box [if wontLeakAnything then
                     string "(uw_begin_region(ctx), "
                 else
                     box [],
                 string "({",
                 newline,
                 p_typ env state,
                 space,
                 string "acc",
                 space,
                 string "=",
                 space,
                 p_exp env initial,
                 string ";",
                 newline,
                 string "int dummy = (uw_begin_region(ctx), 0);",
                 newline,
                 
                 case prepared of
                     NONE =>
                     box [string "char *query = ",
                          p_exp env query,
                          string ";",
                          newline,
                          newline,

                          #query (Settings.currentDbms ())
                                 {loc = loc,
                                  cols = map (fn (_, t) => sql_type_in env t) outputs,
                                  doCols = doCols}]
                   | SOME {id, query, nested} =>
                     box [p_list_sepi newline
                                      (fn i => fn (e, t) =>
                                                  box [p_sql_type t,
                                                       space,
                                                       string "arg",
                                                       string (Int.toString (i + 1)),
                                                       space,
                                                       string "=",
                                                       space,
                                                       p_exp env e,
                                                       string ";"])
                                      inputs,
                          newline,
                          newline,

                          #queryPrepared (Settings.currentDbms ())
                                         {loc = loc,
                                          id = id,
                                          query = query,
                                          inputs = map #2 inputs,
                                          cols = map (fn (_, t) => sql_type_in env t) outputs,
                                          doCols = doCols,
                                          nested = nested}],
                 newline,

                 if wontLeakAnything then
                     box [string "uw_end_region(ctx);",
                          newline]
                 else
                     box [],
                 string "acc;",
                 newline,
                 string "})",
                 if wontLeakAnything then
                     string ")"
                 else
                     box []]
        end

      | EDml {dml, prepared, mode} =>
        box [string "(uw_begin_region(ctx), ({",
             newline,
             case prepared of
                 NONE => box [string "char *dml = ",
                              p_exp env dml,
                              string ";",
                              newline,
                              newline,
                              #dml (Settings.currentDbms ()) (loc, mode)]
               | SOME {id, dml = dml'} =>
                 let
                     val inputs = getPargs dml
                 in
                     box [p_list_sepi newline
                                      (fn i => fn (e, t) =>
                                                  box [p_sql_type t,
                                                       space,
                                                       string "arg",
                                                       string (Int.toString (i + 1)),
                                                       space,
                                                       string "=",
                                                       space,
                                                       p_exp env e,
                                                       string ";"])
                                      inputs,
                          newline,
                          newline,

                          #dmlPrepared (Settings.currentDbms ()) {loc = loc,
                                                                  id = id,
                                                                  dml = dml',
                                                                  inputs = map #2 inputs,
                                                                  mode = mode}]
                 end,
             newline,
             newline,
             string "uw_end_region(ctx);",
             newline,

             case mode of
                 Settings.Error => string "0;"
               | Settings.None => string "uw_dup_and_clear_error_message(ctx);",

             newline,
             string "}))"]

      | ENextval {seq, prepared} =>
        box [string "({",
             newline,
             string "uw_Basis_int n;",
             newline,

             case prepared of
                 NONE => #nextval (Settings.currentDbms ()) {loc = loc,
                                                             seqE = p_exp env seq,
                                                             seqName = case #1 seq of
                                                                           EPrim (Prim.String s) => SOME s
                                                                         | _ => NONE}
               | SOME {id, query} => #nextvalPrepared (Settings.currentDbms ()) {loc = loc,
                                                                                 id = id,
                                                                                 query = query},
             newline,
             newline,

             string "n;",
             newline,
             string "})"]

      | ESetval {seq, count} =>
        box [string "({",
             newline,

             #setval (Settings.currentDbms ()) {loc = loc,
                                                seqE = p_exp env seq,
                                                count = p_exp env count},
             newline,
             newline,

             string "0;",
             newline,
             string "})"]

      | EUnurlify (e, t, true) =>
        let
            fun getIt () =
                if isUnboxable t then
                    unurlify false env t
                else
                    box [string "({",
                         newline,
                         p_typ env t,
                         string " *tmp = uw_malloc(ctx, sizeof(",
                         p_typ env t,
                         string "));",
                         newline,
                         string "*tmp = ",
                         unurlify false env t,
                         string ";",
                         newline,
                         string "tmp;",
                         newline,
                         string "})"]
        in
            box [string "({",
                 newline,
                 string "uw_Basis_string request = uw_maybe_strdup(ctx, ",
                 p_exp env e,
                 string ");",
                 newline,
                 newline,
                 string "(request ? ",
                 getIt (),
                 string " : NULL);",
                 newline,
                 string "})"]
        end

      | EUnurlify (e, t, false) =>
        let
            fun getIt () =
                if isUnboxable t then
                    unurlify false env t
                else
                    box [string "({",
                         newline,
                         p_typ env t,
                         string " *tmp = uw_malloc(ctx, sizeof(",
                         p_typ env t,
                         string "));",
                         newline,
                         string "*tmp = ",
                         unurlify false env t,
                         string ";",
                         newline,
                         string "tmp;",
                         newline,
                         string "})"]
        in
            box [string "({",
                 newline,
                 string "uw_Basis_string request = uw_maybe_strdup(ctx, ",
                 p_exp env e,
                 string ");",
                 newline,
                 newline,
                 unurlify false env t,
                 string ";",
                 newline,
                 string "})"]
        end

and p_exp env = p_exp' false env

fun p_fun isRec env (fx, n, args, ran, e) =
    let
        val nargs = length args
        val env' = foldl (fn ((x, dom), env) => E.pushERel env x dom) env args
    in
        box [string "static",
             space,
             p_typ env ran,
             space,
             string ("__uwn_" ^ ident fx ^ "_" ^ Int.toString n),
             string "(",
             p_list_sep (box [string ",", space]) (fn x => x)
                        (string "uw_context ctx" :: ListUtil.mapi (fn (i, (_, dom)) =>
                                                                      box [p_typ env dom,
                                                                           space,
                                                                           p_rel env' (nargs - i - 1)]) args),
             string ")",
             space,
             string "{",
             newline,
             if isRec andalso Settings.getDeadlines () then
                 box [string "uw_check_deadline(ctx);",
                      newline]
             else
                 box [],
             box [string "return(",
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
                 string ("__uws_" ^ Int.toString n),
                 space,
                 string "{",
                 newline,
                 p_list_sep (box []) (fn (x, t) => box [p_typ env t,
                                                        space,
                                                        string "__uwf_",
                                                        p_ident x,
                                                        string ";",
                                                        newline]) xts,
                 string "};"]
        end
      | DDatatype dts =>
        let
            val dts = ListMergeSort.sort (fn ((dk1, _, _, _), (dk2, _, _, _)) =>
                                             dk1 = Enum andalso dk2 <> Enum) dts

            fun p_one (Enum, x, n, xncs) =
                box [string "enum",
                     space,
                     string ("__uwe_" ^ ident x ^ "_" ^ Int.toString n),
                     space,
                     string "{",
                     space,
                     case xncs of
                         [] => string ("__uwec_" ^ ident x ^ "_" ^ Int.toString n)
                       | _ =>
                         p_list_sep (box [string ",", space]) (fn (x, n, _) =>
                                                                  string ("__uwc_" ^ ident x ^ "_" ^ Int.toString n)) xncs,
                     space,
                     string "};"]
              | p_one (Option, _, _, _) = box []
              | p_one (Default, x, n, xncs) =
                let
                    val xncsArgs = List.mapPartial (fn (x, n, NONE) => NONE
                                                     | (x, n, SOME t) => SOME (x, n, t)) xncs
                in
                    box [string "enum",
                         space,
                         string ("__uwe_" ^ ident x ^ "_" ^ Int.toString n),
                         space,
                         string "{",
                         space,
                         p_list_sep (box [string ",", space]) (fn (x, n, _) =>
                                                                  string ("__uwc_" ^ ident x ^ "_" ^ Int.toString n))
                                    xncs,
                         space,
                         string "};",
                         newline,
                         newline,
                         string "struct",
                         space,
                         string ("__uwd_" ^ ident x ^ "_" ^ Int.toString n),
                         space,
                         string "{",
                         newline,
                         string "enum",
                         space,
                         string ("__uwe_" ^ ident x ^ "_" ^ Int.toString n),
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
                                                                                 string ("uw_" ^ ident x),
                                                                                 string ";"]) xncsArgs,
                                        newline,
                                        string "}",
                                        space,
                                        string "data;",
                                        newline]),
                         string "};"]
                end
        in
            p_list_sep (box []) p_one dts
        end

      | DDatatypeForward _ => box []

      | DVal (x, n, t, e) =>
        box [p_typ env t,
             space,
             string ("__uwn_" ^ ident x ^ "_" ^ Int.toString n),
             space,
             string "=",
             space,
             p_exp env e,
             string ";"]
      | DFun vi => p_fun false env vi
      | DFunRec vis =>
        let
            val env = E.declBinds env dAll
        in
            box [p_list_sep newline (fn (fx, n, args, ran, _) =>
                                        box [string "static",
                                             space,
                                             p_typ env ran,
                                             space,
                                             string ("__uwn_" ^ ident fx ^ "_" ^ Int.toString n),
                                             string "(uw_context,",
                                             space,
                                             p_list_sep (box [string ",", space])
                                                        (fn (_, dom) => p_typ env dom) args,
                                             string ");"]) vis,
                 newline,
                 p_list_sep newline (p_fun true env) vis,
                 newline]
        end
      | DTable (x, _, pk, csts) => box [string "/* SQL table ",
                                        string x,
                                        space,
                                        case pk of
                                            "" => box []
                                          | _ => box [string "keys",
                                                      space,
                                                      string pk,
                                                      space],
                                        string "constraints",
                                        space,
                                        p_list (fn (x, v) => box [string x,
                                                                  space,
                                                                  string ":",
                                                                  space,
                                                                  string v]) csts,
                                        space,
                                        string " */",
                                        newline]
      | DSequence x => box [string "/* SQL sequence ",
                            string x,
                            string " */",
                            newline]
      | DView (x, _, s) => box [string "/* SQL view ",
                                string x,
                                space,
                                string "AS",
                                space,
                                string s,
                                space,
                                string " */",
                                newline]
      | DDatabase _ => box []
      | DPreparedStatements _ => box []

      | DJavaScript s => box [string "static char jslib[] = \"",
                              string (String.toCString s),
                              string "\";"]
      | DCookie s => box [string "/*",
                          space,
                          string "cookie",
                          space,
                          string s,
                          space,
                          string "*/"]
      | DStyle s => box [string "/*",
                         space,
                         string "style",
                         space,
                         string s,
                         space,
                         string "*/"]

      | DTask _ => box []
      | DOnError _ => box []

datatype 'a search =
         Found of 'a
       | NotFound
       | Error

fun p_sqltype'' env (tAll as (t, loc)) =
    case t of
        TFfi ("Basis", "int") => "int8"
      | TFfi ("Basis", "float") => "float8"
      | TFfi ("Basis", "string") => "text"
      | TFfi ("Basis", "bool") => "bool"
      | TFfi ("Basis", "time") => "timestamp"
      | TFfi ("Basis", "blob") => "bytea"
      | TFfi ("Basis", "channel") => "int8"
      | TFfi ("Basis", "client") => "int4"
      | _ => (ErrorMsg.errorAt loc "Don't know SQL equivalent of type";
              Print.eprefaces' [("Type", p_typ env tAll)];
              "ERROR")

fun p_sqltype' env (tAll as (t, loc)) =
    case t of
        (TOption t, _) => p_sqltype'' env t
      | _ => p_sqltype'' env t ^ " NOT NULL"

fun p_sqltype env t = string (p_sqltype' env t)

fun p_sqltype_base' env t =
    case t of
        (TOption t, _) => p_sqltype'' env t
      | _ => p_sqltype'' env t

fun p_sqltype_base env t = string (p_sqltype_base' env t)

fun is_not_null t =
    case t of
        (TOption _, _) => false
      | _ => true

fun sigName fields =
    let
        fun inFields s = List.exists (fn (s', _) => s' = s) fields

        fun getSigName n =
            let
                val s = "Sig" ^ Int.toString n
            in
                if inFields s then
                    getSigName (n + 1)
                else
                    s
            end
    in
        if inFields "Sig" then
            getSigName 0
        else
            "Sig"
    end

fun p_file env (ds, ps) =
    let
        val (pds, env) = ListUtil.foldlMap (fn (d, env) =>
                                               (p_decl env d,
                                                E.declBinds env d))
                             env ds

        fun flatFields always (t : typ) =
            case #1 t of
                TRecord i =>
                let
                    val xts = E.lookupStruct env i
                in
                    SOME ((always @ map #1 xts) :: List.concat (List.mapPartial (flatFields [] o #2) xts))
                end
              | TList (_, i) =>
                let
                    val ts = E.lookupStruct env i
                in
                    case ts of
                        [("1", t'), ("2", _)] => flatFields [] t'
                      | _ => raise Fail "CjrPrint: Bad struct for TList"
                end
              | _ => NONE

        val fields = foldl (fn ((ek, _, _, ts, _, _, _), fields) =>
                               case ek of
                                   Action eff =>
                                   (case List.nth (ts, length ts - 2) of
                                        (TRecord i, loc) =>
                                        let
                                            val xts = E.lookupStruct env i
                                            val extra = case eff of
                                                            ReadCookieWrite => [sigName xts]
                                                          | _ => []
                                        in
                                            case flatFields extra (TRecord i, loc) of
                                                NONE => raise Fail "CjrPrint: flatFields impossible"
                                              | SOME fields' => List.revAppend (fields', fields)
                                        end
                                      | _ => raise Fail "CjrPrint: Last argument of action isn't record")
                                 | _ => fields)
                           [] ps

        val fields = foldl (fn (xts, fields) =>
                               let
                                   val xtsSet = SS.addList (SS.empty, xts)
                               in
                                   foldl (fn (x, fields) =>
                                             let
                                                 val xtsSet' = Option.getOpt (SM.find (fields, x), SS.empty)
                                             in
                                                 SM.insert (fields, x, SS.union (SS.delete (xtsSet, x),
                                                                                 xtsSet'))
                                             end) fields xts
                               end)
                           SM.empty fields

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

        val cookies = List.mapPartial (fn (DCookie s, _) => SOME s | _ => NONE) ds

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

        fun getInput (x, t) =
            let
                val n = case SM.find (fnums, x) of
                            NONE => raise Fail ("CjrPrint: Can't find " ^ x ^ " in fnums")
                          | SOME n => n

                val f = case t of
                            (TFfi ("Basis", "bool"), _) => "optional_"
                          | _ => ""
            in
                if isFile t then
                    box [string "uw_input_",
                         p_ident x,
                         space,
                         string "=",
                         space,
                         string "uw_get_file_input(ctx, ",
                         string (Int.toString n),
                         string ");",
                         newline]
                else case #1 t of
                         TRecord i =>
                         let
                             val xts = E.lookupStruct env i
                         in
                             box [string "uw_enter_subform(ctx, ",
                                  string (Int.toString n),
                                  string ");",
                                  newline,
                                  string "uw_input_",
                                  p_ident x,
                                  space,
                                  string "=",
                                  space,
                                  string "({",
                                  box [p_typ env t,
                                       space,
                                       string "result;",
                                       newline,
                                       p_list_sep (box [])
                                                  (fn (x, t) =>
                                                      box [p_typ env t,
                                                           space,
                                                           string "uw_input_",
                                                           string x,
                                                           string ";",
                                                           newline])
                                                  xts,
                                       newline,
                                       p_list_sep (box []) (fn (x, t) =>
                                                               box [getInput (x, t),
                                                                    string "result.__uwf_",
                                                                    string x,
                                                                    space,
                                                                    string "=",
                                                                    space,
                                                                    string "uw_input_",
                                                                    string x,
                                                                    string ";",
                                                                    newline])
                                                  xts,
                                       newline,
                                       string "result;",
                                       newline],
                                  string "});",
                                  newline,
                                  string "uw_leave_subform(ctx);"]
                         end
                       | TList (t', i) =>
                         let
                             val xts = E.lookupStruct env i
                             val i' = case xts of
                                          [("1", (TRecord i', loc)), ("2", _)] => i'
                                        | _ => raise Fail "CjrPrint: Bad TList record [2]"
                             val xts = E.lookupStruct env i'
                         in
                             box [string "{",
                                  newline,
                                  string "int status;",
                                  newline,
                                  string "uw_input_",
                                  p_ident x,
                                  space,
                                  string "=",
                                  space,
                                  string "NULL;",
                                  newline,
                                  string "for (status = uw_enter_subforms(ctx, ",
                                  string (Int.toString n),
                                  string "); status; status = uw_next_entry(ctx)) {",
                                  newline,
                                  box [p_typ env t,
                                       space,
                                       string "result",
                                       space,
                                       string "=",
                                       space,
                                       string "uw_malloc(ctx, sizeof(struct __uws_",
                                       string (Int.toString i),
                                       string "));",
                                       newline,
                                       box [string "{",
                                            p_list_sep (box [])
                                                       (fn (x, t) =>
                                                           box [p_typ env t,
                                                                space,
                                                                string "uw_input_",
                                                                string x,
                                                                string ";",
                                                                newline])
                                                       xts,
                                            newline,
                                            p_list_sep (box []) (fn (x, t) =>
                                                                    box [getInput (x, t),
                                                                         string "result->__uwf_1.__uwf_",
                                                                         string x,
                                                                         space,
                                                                         string "=",
                                                                         space,
                                                                         string "uw_input_",
                                                                         string x,
                                                                         string ";",
                                                                         newline])
                                                       xts,
                                            string "}",
                                            newline],
                                       newline,
                                       string "result->__uwf_2 = uw_input_",
                                       p_ident x,
                                       string ";",
                                       newline,
                                       string "uw_input_",
                                       p_ident x,
                                       string " = result;",
                                       newline],
                                  string "}}",
                                  newline]
                         end
                       | _ =>
                         box [string "request = uw_get_",
                              string f,
                              string "input(ctx, ",
                              string (Int.toString n),
                              string ");",
                              newline,
                              string "if (request == NULL)",
                              newline,
                              box [string "uw_error(ctx, FATAL, \"Missing input ",
                                   string x,
                                   string "\");"],
                              newline,
                              string "uw_input_",
                              p_ident x,
                              space,
                              string "=",
                              space,
                              unurlify true env t,
                              string ";",
                              newline]
            end

        fun p_page (ek, s, n, ts, ran, side, tellSig) =
            let
                val (ts, defInputs, inputsVar, fields) =
                    case ek of
                        Core.Action _ =>
                        (case List.nth (ts, length ts - 2) of
                             (TRecord i, _) =>
                             let
                                 val xts = E.lookupStruct env i
                             in
                                 (List.take (ts, length ts - 2),
                                  box [box (map (fn (x, t) => box [p_typ env t,
                                                                   space,
                                                                   string "uw_input_",
                                                                   p_ident x,
                                                                   string ";",
                                                                   newline]) xts),
                                       newline,
                                       box (map getInput xts),
                                       case i of
                                           0 => string "uw_unit uw_inputs;"
                                         | _ => box [string "struct __uws_",
                                                     string (Int.toString i),
                                                     space,
                                                     string "uw_inputs",
                                                     space,
                                                     string "= {",
                                                     newline,
                                                     box (map (fn (x, _) => box [string "uw_input_",
                                                                                 p_ident x,
                                                                                 string ",",
                                                                                 newline]) xts),
                                                     string "};"],
                                       newline],
                                  box [string ",",
                                       space,
                                       string "uw_inputs"],
                                  SOME xts)
                             end

                           | _ => raise Fail "CjrPrint: Last argument to an action isn't a record")
                      | _ => (List.take (ts, length ts - 1), string "", string "", NONE)

                fun couldWrite ek =
                    case ek of
                        Link => false
                      | Action ef => ef = ReadCookieWrite
                      | Rpc ef => ef = ReadCookieWrite
                      | Extern _ => false

                val s =
                    case Settings.getUrlPrefix () of
                        "" => s
                      | "/" => s
                      | prefix =>
                        if size s > 0 andalso String.sub (s, 0) = #"/" then
                            prefix ^ String.extract (s, 1, NONE)
                        else
                            prefix ^ s
            in
                box [string "if (!strncmp(request, \"",
                     string (String.toCString s),
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
                     if couldWrite ek andalso not (Settings.checkNoXsrfProtection s) then
                         box [string "{",
                              newline,
                              string "uw_Basis_string sig = ",
                              case fields of
                                  NONE => string "uw_Basis_requestHeader(ctx, \"UrWeb-Sig\")"
                                | SOME fields =>
                                  case SM.find (fnums, sigName fields) of
                                      NONE => raise Fail "CjrPrint: sig name wasn't assigned a number"
                                    | SOME inum =>
                                      string ("uw_get_input(ctx, " ^ Int.toString inum ^ ")"),
                              string ";",
                              newline,
                              string "if (sig == NULL) uw_error(ctx, FATAL, \"Missing cookie signature\");",
                              newline,
                              string "if (strcmp(sig, uw_cookie_sig(ctx)))",
                              newline,
                              box [string "uw_error(ctx, FATAL, \"Wrong cookie signature\");",
                                   newline],
                              string "}",
                              newline]
                     else
                         box [],
                     box (case ek of
                              Core.Rpc _ => [string "uw_write_header(ctx, \"Content-type: text/plain\\r\\n\");",
                                             newline]
                            | _ => [string "uw_write_header(ctx, \"Content-type: text/html\\r\\n\");",
                                    newline,
                                    string "uw_write_header(ctx, \"Content-script-type: text/javascript\\r\\n\");",
                                    newline,
                                    string "uw_write(ctx, begin_xhtml);",
                                    newline,
                                    string "uw_mayReturnIndirectly(ctx);",
                                    newline,
                                    string "uw_set_script_header(ctx, \"",
                                    let
                                        val scripts =
                                            case side of
                                                ServerOnly => ""
                                              | _ =>
                                                let
                                                    val scripts =
                                                        "<script type=\\\"text/javascript\\\" src=\\\""
                                                        ^ OS.Path.joinDirFile {dir = Settings.getUrlPrefix (),
                                                                               file = "app.js"}
                                                        ^ "\\\"></script>\\n"
                                                in
                                                    foldl (fn (x, scripts) =>
                                                              scripts
                                                              ^ "<script type=\\\"text/javascript\\\" src=\\\"" ^ x ^ "\\\"></script>\\n")
                                                          scripts (Settings.getScripts ())
                                                end
                                    in
                                        string scripts
                                    end,
                                    string "\");",
                                    newline]),
                     string "uw_set_needs_push(ctx, ",
                     string (case side of
                                 ServerAndPullAndPush => "1"
                               | _ => "0"),
                     string ");",
                     newline,
                     string "uw_set_needs_sig(ctx, ",
                     string (if tellSig then
                                 "1"
                             else
                                 "0"),
                     string ");",
                     newline,
                     string "uw_login(ctx);",
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
                                                                case #1 t of
                                                                    TFfi ("Basis", "postBody") => string "uw_getPostBody(ctx)"
                                                                  | TOption (TFfi ("Basis", "queryString"), _) => string "uw_queryString(ctx)"
                                                                  | _ => unurlify false env t,
                                                                string ";",
                                                                newline]) ts),
                          defInputs,
                          box (case ek of
                                   Core.Rpc _ => [p_typ env ran,
                                                  space,
                                                  string "it0",
                                                  space,
                                                  string "=",
                                                  space]
                                 | _ => []),
                          p_enamed env n,
                          string "(",
                          p_list_sep (box [string ",", space])
                                     (fn x => x)
                                     (string "ctx"
                                      :: ListUtil.mapi (fn (i, _) => string ("arg" ^ Int.toString i)) ts),
                          inputsVar,
                          string ", 0);",
                          newline,
                          box (case ek of
                                   Core.Rpc _ => [string "uw_write(ctx, uw_get_real_script(ctx));",
                                                  newline,
                                                  string "uw_write(ctx, \"\\n\");",
                                                  newline,
                                                  urlify env ran]
                                 | _ => [string "uw_write(ctx, \"</html>\");",
                                         newline]),
                          string "return;",
                          newline,
                          string "}",
                          newline,
                          string "}"]
                    ]
            end

        val pds' = map p_page ps

        val hasDb = ref false
        val tables = ref []
        val views = ref []
        val sequences = ref []
        val dbstring = ref ""
        val expunge = ref 0
        val initialize = ref 0
        val prepped = ref []

        val () = app (fn d =>
                         case #1 d of
                             DDatabase {name = x, expunge = y, initialize = z} => (hasDb := true;
                                                                                   dbstring := x;
                                                                                   expunge := y;
                                                                                   initialize := z)
                           | DTable (s, xts, _, _) => tables := (s, map (fn (x, t) =>
                                                                            (x, sql_type_in env t)) xts) :: !tables
                           | DView (s, xts, _) => views := (s, map (fn (x, t) =>
                                                                       (x, sql_type_in env t)) xts) :: !views
                           | DSequence s => sequences := s :: !sequences
                           | DPreparedStatements ss => prepped := ss
                           | _ => ()) ds

        val hasDb = !hasDb

        fun expDb (e, _) =
            case e of
                ECon (_, _, SOME e) => expDb e
              | ESome (_, e) => expDb e
              | EFfiApp (_, _, es) => List.exists expDb es
              | EApp (e, es) => expDb e orelse List.exists expDb es
              | EUnop (_, e) => expDb e
              | EBinop (_, e1, e2) => expDb e1 orelse expDb e2
              | ERecord (_, xes) => List.exists (expDb o #2) xes
              | EField (e, _) => expDb e
              | ECase (e, pes, _) => expDb e orelse List.exists (expDb o #2) pes
              | EError (e, _) => expDb e
              | EReturnBlob {blob = e1, mimeType = e2, ...} => expDb e1 orelse expDb e2
              | ERedirect (e, _) => expDb e
              | EWrite e => expDb e
              | ESeq (e1, e2) => expDb e1 orelse expDb e2
              | ELet (_, _, e1, e2) => expDb e1 orelse expDb e2
              | EQuery _ => true
              | EDml _ => true
              | ENextval _ => true
              | ESetval _ => true
              | EUnurlify (e, _, _) => expDb e
              | _ => false

        fun declDb (d, _) =
            case d of
                DVal (_, _, _, e) => expDb e
              | DFun (_, _, _, _, e) => expDb e
              | DFunRec vis => List.exists (expDb o #5) vis
              | _ => false

        val () = if not hasDb andalso List.exists declDb ds then
                     ErrorMsg.error "Application uses a database but has none configured with 'database' in .urp file."
                 else
                     ()

        val cookies = List.mapPartial (fn (DCookie s, _) => SOME s | _ => NONE) ds

        val cookieCode = foldl (fn (cookie, acc) =>
                                   SOME (case acc of
                                             NONE => string ("uw_unnull(uw_Basis_get_cookie(ctx, \""
                                                             ^ cookie ^ "\"))")
                                           | SOME acc => box [string ("uw_Basis_strcat(ctx, uw_unnull(uw_Basis_get_cookie(ctx, \""
                                                                      ^ cookie ^ "\")), uw_Basis_strcat(ctx, \"/\", "),
                                                              acc,
                                                              string "))"]))
                         NONE cookies

        fun makeChecker (name, rules : Settings.rule list) =
            box [string "static int ",
                 string name,
                 string "(const char *s) {",
                 newline,
                 box [p_list_sep (box [])
                      (fn rule =>
                          box [string "if (!str",
                               case #kind rule of
                                   Settings.Exact => box [string "cmp(s, \"",
                                                          string (String.toCString (#pattern rule)),
                                                          string "\"))"]
                                 | Settings.Prefix => box [string "ncmp(s, \"",
                                                           string (String.toCString (#pattern rule)),
                                                           string "\", ",
                                                           string (Int.toString (size (#pattern rule))),
                                                           string "))"],
                               string " return ",
                               string (case #action rule of
                                           Settings.Allow => "1"
                                         | Settings.Deny => "0"),
                               string ";",
                               newline]) rules,
                      string "return 0;",
                      newline],
                 string "}",
                 newline]

        val initializers = List.mapPartial (fn (DTask (Initialize, x1, x2, e), _) => SOME (x1, x2, e) | _ => NONE) ds
        val expungers = List.mapPartial (fn (DTask (ClientLeaves, x1, x2, e), _) => SOME (x1, x2, e) | _ => NONE) ds
        val periodics = List.mapPartial (fn (DTask (Periodic n, x1, x2, e), _) => SOME (n, x1, x2, e) | _ => NONE) ds

        val onError = ListUtil.search (fn (DOnError n, _) => SOME n | _ => NONE) ds

        val now = Time.now ()
        val nowD = Date.fromTimeUniv now
        val rfcFmt = "%a, %d %b %Y %H:%M:%S"
    in
        box [string "#include \"",
             string (OS.Path.joinDirFile {dir = Config.includ,
                                          file = "config.h"}),
             string "\"",
             newline,
             string "#include <stdio.h>",
             newline,
             string "#include <stdlib.h>",
             newline,
             string "#include <string.h>",
             newline,
             string "#include <math.h>",
             newline,
             string "#include <time.h>",
             newline,
             if hasDb then
                 box [string ("#include <" ^ #header (Settings.currentDbms ()) ^ ">"),
                      newline]
             else
                 box [],
             p_list_sep (box []) (fn s => box [string "#include \"",
                                               string s,
                                               string "\"",
                                               newline]) (Settings.getHeaders ()),
             string "#include \"",
             string (OS.Path.joinDirFile {dir = Config.includ,
                                          file = "urweb.h"}),
             string "\"",
             newline,
             newline,

             box [string "static void uw_setup_limits() {",
                  newline,
                  case Settings.getMinHeap () of
                      0 => box []
                    | n => box [string "uw_min_heap",
                                space,
                                string "=",
                                space,
                                string (Int.toString n),
                                string ";",
                                newline,
                                newline],
                  box [p_list_sep (box []) (fn (class, num) =>
                                               let
                                                   val num = case class of
                                                                 "page" => Int.max (2048, num)
                                                               | _ => num
                                               in
                                                   box [string ("uw_" ^ class ^ "_max"),
                                                        space,
                                                        string "=",
                                                        space,
                                                        string (Int.toString num),
                                                        string ";",
                                                        newline]
                                               end) (Settings.limits ())],
                  string "}",
                  newline,
                  newline],

             #code (Settings.currentProtocol ()) (),

             if hasDb then
                 #init (Settings.currentDbms ()) {dbstring = !dbstring,
                                                  prepared = !prepped,
                                                  tables = !tables,
                                                  views = !views,
                                                  sequences = !sequences}
             else
                 box [string "static void uw_client_init(void) { };",
                      newline,
                      string "static void uw_db_init(uw_context ctx) { };",
                      newline,
                      string "static int uw_db_begin(uw_context ctx) { return 0; };",
                      newline,
                      string "static void uw_db_close(uw_context ctx) { };",
                      newline,
                      string "static int uw_db_commit(uw_context ctx) { return 0; };",
                      newline,
                      string "static int uw_db_rollback(uw_context ctx) { return 0; };"],
             newline,
             newline,

             string "static const char begin_xhtml[] = \"<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\" ?>\\n<!DOCTYPE html PUBLIC \\\"-//W3C//DTD XHTML 1.0 Transitional//EN\\\" \\\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\\\">\\n<html xmlns=\\\"http://www.w3.org/1999/xhtml\\\" xml:lang=\\\"en\\\" lang=\\\"en\\\">\";",
             newline,
             newline,

             p_list_sep newline (fn x => x) pds,
             newline,
             newline,
             string "static int uw_input_num(const char *name) {",
             newline,
             makeSwitch (fnums, 0),
             string "}",
             newline,
             newline,

             box (ListUtil.mapi (fn (i, (_, x1, x2, e)) =>
                                    box [string "static void uw_periodic",
                                         string (Int.toString i),
                                         string "(uw_context ctx) {",
                                         newline,
                                         box [string "uw_unit __uwr_",
                                              string x1,
                                              string "_0 = 0, __uwr_",
                                              string x2,
                                              string "_1 = 0;",
                                              newline,
                                              p_exp (E.pushERel (E.pushERel env x1 dummyt) x2 dummyt) e,
                                              string ";",
                                              newline],
                                         string "}",
                                         newline,
                                         newline]) periodics),

             string "static uw_periodic my_periodics[] = {",
             box (ListUtil.mapi (fn (i, (n, _, _, _)) =>
                                    box [string "{uw_periodic",
                                         string (Int.toString i),
                                         string ",",
                                         space,
                                         string (Int64.toString n),
                                         string "},"]) periodics),
             string "{NULL}};",
             newline,
             newline,

             makeChecker ("uw_check_url", Settings.getUrlRules ()),
             newline,

             makeChecker ("uw_check_mime", Settings.getMimeRules ()),
             newline,

             makeChecker ("uw_check_requestHeader", Settings.getRequestHeaderRules ()),
             newline,

             makeChecker ("uw_check_responseHeader", Settings.getResponseHeaderRules ()),
             newline,
             
             string "extern void uw_sign(const char *in, char *out);",
             newline,
             string "extern int uw_hash_blocksize;",
             newline,
             string "static uw_Basis_string uw_cookie_sig(uw_context ctx) {",
             newline,
             box [string "uw_Basis_string r = uw_malloc(ctx, uw_hash_blocksize);",
                  newline,
                  string "uw_sign(",
                  case cookieCode of
                      NONE => string "\"\""
                    | SOME code => code,
                  string ", r);",
                  newline,
                  string "return uw_Basis_makeSigString(ctx, r);",
                  newline],
             string "}",
             newline,
             newline,

             string "static void uw_handle(uw_context ctx, char *request) {",
             newline,
             string "if (!strcmp(request, \"",
             string (OS.Path.joinDirFile {dir = Settings.getUrlPrefix (),
                                          file = "app.js"}),
             string "\")) {",
             newline,
             box [string "uw_Basis_string ims = uw_Basis_requestHeader(ctx, \"If-modified-since\");",
                  newline,
                  string ("if (ims && !strcmp(ims, \"" ^ Date.fmt rfcFmt nowD ^ "\")) {"),
                  newline,
                  box [string "uw_clear_headers(ctx);",
                       newline,
                       string "uw_write_header(ctx, uw_supports_direct_status ? \"HTTP/1.1 304 Not Modified\\r\\n\" : \"Status: 304 Not Modified\\r\\n\");",
                       newline,
                       string "return;",
                       newline],
                  string "}",
                  newline,
                  newline,
                  string "uw_write_header(ctx, \"Content-type: text/javascript\\r\\n\");",
                  newline,
                  string ("uw_write_header(ctx, \"Last-modified: " ^ Date.fmt rfcFmt nowD ^ "\\r\\n\");"),
                  newline,
                  string "uw_write(ctx, jslib);",
                  newline,
                  string "return;",
                  newline],
             string "}",
             newline,
             p_list_sep newline (fn x => x) pds',
             newline,
             string "uw_clear_headers(ctx);",
             newline,
             string "uw_write_header(ctx, \"HTTP/1.1 404 Not Found\\r\\nContent-type: text/plain\\r\\n\");",
             newline,
             string "uw_write(ctx, \"Not Found\");",
             newline,
             string "}",
             newline,
             newline,

             box [string "static void uw_expunger(uw_context ctx, uw_Basis_client cli) {",
                  newline,

                  p_list_sep (box []) (fn (x1, x2, e) => box [string "({",
                                                              newline,
                                                              string "uw_Basis_client __uwr_",
                                                              string x1,
                                                              string "_0 = cli;",
                                                              newline,
                                                              string "uw_unit __uwr_",
                                                              string x2,
                                                              string "_1 = 0;",
                                                              newline,
                                                              p_exp (E.pushERel (E.pushERel env x1 (TFfi ("Basis", "client"), ErrorMsg.dummySpan))
                                                                                x2 dummyt) e,
                                                              string ";",
                                                              newline,
                                                              string "});",
                                                              newline]) expungers,

                  if hasDb then
                      box [p_enamed env (!expunge),
                           string "(ctx, cli);",
                           newline]
                  else
                      box [],
                  string "}"],

             newline,
             string "static void uw_initializer(uw_context ctx) {",
             newline,
             box [p_list_sep (box []) (fn (x1, x2, e) => box [string "({",
                                                              newline,
                                                              string "uw_unit __uwr_",
                                                              string x1,
                                                              string "_0 = 0, __uwr_",
                                                              string x2,
                                                              string "_1 = 0;",
                                                              newline,
                                                              p_exp (E.pushERel (E.pushERel env x1 dummyt) x2 dummyt) e,
                                                              string ";",
                                                              newline,
                                                              string "});",
                                                              newline]) initializers,
                  if hasDb then
                      box [p_enamed env (!initialize),
                           string "(ctx, 0);",
                           newline]
                  else
                      box []],
             string "}",
             newline,

             case onError of
                 NONE => box []
               | SOME n => box [string "static void uw_onError(uw_context ctx, char *msg) {",
                                newline,
                                box [string "uw_write(ctx, ",
                                     p_enamed env n,
                                     string "(ctx, msg, 0));",
                                     newline],
                                string "}",
                                newline,
                                newline],

             string "uw_app uw_application = {",
             p_list_sep (box [string ",", newline]) string
                        [Int.toString (SM.foldl Int.max 0 fnums + 1),
                         Int.toString (Settings.getTimeout ()),
                         "\"" ^ Settings.getUrlPrefix () ^ "\"",
                         "uw_client_init", "uw_initializer", "uw_expunger",
                         "uw_db_init", "uw_db_begin", "uw_db_commit", "uw_db_rollback", "uw_db_close",
                         "uw_handle",
                         "uw_input_num", "uw_cookie_sig", "uw_check_url", "uw_check_mime", "uw_check_requestHeader", "uw_check_responseHeader",
                         case onError of NONE => "NULL" | SOME _ => "uw_onError", "my_periodics"],
             string "};",
             newline]
    end

fun p_sql env (ds, _) =
    let
        val (pps, _) = ListUtil.foldlMap
                       (fn (dAll as (d, _), env) =>
                           let
                               val pp = case d of
                                            DTable (s, xts, pk, csts) =>
                                            box [string "CREATE TABLE ",
                                                 string s,
                                                 string "(",
                                                 p_list (fn (x, t) =>
                                                            let
                                                                val t = sql_type_in env t
                                                            in
                                                                box [string "uw_",
                                                                     string (CharVector.map Char.toLower x),
                                                                     space,
                                                                     string (#p_sql_type (Settings.currentDbms ()) t),
                                                                     case t of
                                                                         Nullable _ => box []
                                                                       | _ => string " NOT NULL"]
                                                            end) xts,
                                                 case (pk, csts) of
                                                     ("", []) => box []
                                                   | _ => string ",",
                                                 cut,
                                                 case pk of
                                                     "" => box []
                                                   | _ => box [string "PRIMARY",
                                                               space,
                                                               string "KEY",
                                                               space,
                                                               string "(",
                                                               string pk,
                                                               string ")",
                                                               case csts of
                                                                   [] => box []
                                                                 | _ => string ",",
                                                               newline],
                                                 p_list_sep (box [string ",", newline])
                                                            (fn (x, c) =>
                                                                box [string "CONSTRAINT",
                                                                     space,
                                                                     string s,
                                                                     string "_",
                                                                     string x,
                                                                     space,
                                                                     string c]) csts,
                                                 newline,
                                                 string ");",
                                                 newline,
                                                 newline]
                                          | DSequence s =>
                                            box [string (#createSequence (Settings.currentDbms ()) s),
                                                 string ";",
                                                 newline,
                                                 newline]
                                          | DView (s, xts, q) =>
                                            box [string "CREATE VIEW",
                                                 space,
                                                 string s,
                                                 space,
                                                 string "AS",
                                                 space,
                                                 string q,
                                                 string ";",
                                                 newline,
                                                 newline]
                                          | _ => box []
                           in
                               (pp, E.declBinds env dAll)
                           end)
                       env ds
    in
        box (string (#sqlPrefix (Settings.currentDbms ())) :: pps)
    end

end
