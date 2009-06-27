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
      | _ => false

fun p_typ' par env (t, loc) =
    case t of
        TFun (t1, t2) => (EM.errorAt loc "Function type remains";
                          string "<FUNCTION>")
      | TRecord i => box [string "struct",
                          space,
                          string "__uws_",
                          string (Int.toString i)]
      | TDatatype (Enum, n, _) =>
        (box [string "enum",
              space,
              string ("__uwe_" ^ #1 (E.lookupDatatype env n) ^ "_" ^ Int.toString n)]
         handle CjrEnv.UnboundNamed _ => string ("__uwd_UNBOUND__" ^ Int.toString n))
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
              string ("__uwd_" ^ #1 (E.lookupDatatype env n) ^ "_" ^ Int.toString n ^ "*")]
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

fun p_enamed env n =
    string ("__uwn_" ^ ident (#1 (E.lookupENamed env n)) ^ "_" ^ Int.toString n)
    handle CjrEnv.UnboundNamed _ => string ("__uwn_UNBOUND_" ^ Int.toString n)

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

fun p_pat (env, exit, depth) (p, loc) =
    case p of
        PWild =>
        (box [], env)
      | PVar (x, t) =>
        (box [string "__uwr_",
              p_ident x,
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
              Prim.p_t_GCC (Prim.Int n),
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
              Prim.p_t_GCC (Prim.String s),
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
                                              ("uw_" ^ ident x, to)
                                          end
                                        | PConFfi {mod = m, con, arg, ...} =>
                                          ("uw_" ^ ident m ^ "_" ^ ident con, arg)

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
                                  if isUnboxable t then
                                      box [string "disc",
                                           string (Int.toString depth)]
                                  else
                                      box [string "*disc",
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
                                                       string ".__uwf_",
                                                       p_ident x,
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

      | PNone t =>
        (box [string "if",
              space,
              string "(disc",
              string (Int.toString depth),
              space,
              string "!=",
              space,
              string "NULL)",
              space,
              exit,
              newline],
         env)

      | PSome (t, p) =>
        let
            val (p, env) =
                let
                    val (p, env) = p_pat (env, exit, depth + 1) p
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
                          if isUnboxable t then
                              box [string "disc",
                                   string (Int.toString depth)]
                          else
                              box [string "*disc",
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
                  space,
                  string "==",
                  space,
                  string "NULL)",
                  space,
                  exit,
                  newline,
                  p],
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

datatype sql_type =
         Int
       | Float
       | String
       | Bool
       | Time
       | Blob
       | Channel
       | Client
       | Nullable of sql_type

fun isBlob Blob = true
  | isBlob (Nullable t) = isBlob t
  | isBlob _ = false

fun isFile (t : typ) =
    case #1 t of
        TFfi ("Basis", "file") => true
      | _ => false

fun p_sql_type' t =
    case t of
        Int => "uw_Basis_int"
      | Float => "uw_Basis_float"
      | String => "uw_Basis_string"
      | Bool => "uw_Basis_bool"
      | Time => "uw_Basis_time"
      | Blob => "uw_Basis_blob"
      | Channel => "uw_Basis_channel"
      | Client => "uw_Basis_client"
      | Nullable String => "uw_Basis_string"
      | Nullable t => p_sql_type' t ^ "*"

fun p_sql_type t = string (p_sql_type' t)

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

fun p_ensql t e =
    case t of
        Int => box [string "uw_Basis_attrifyInt(ctx, ", e, string ")"]
      | Float => box [string "uw_Basis_attrifyFloat(ctx, ", e, string ")"]
      | String => e
      | Bool => box [string "(", e, string " ? \"TRUE\" : \"FALSE\")"]
      | Time => box [string "uw_Basis_attrifyTime(ctx, ", e, string ")"]
      | Blob => box [e, string ".data"]
      | Channel => box [string "uw_Basis_attrifyChannel(ctx, ", e, string ")"]
      | Client => box [string "uw_Basis_attrifyClient(ctx, ", e, string ")"]
      | Nullable String => e
      | Nullable t => box [string "(",
                           e,
                           string " == NULL ? NULL : ",
                           p_ensql t (box [string "(*", e, string ")"]),
                           string ")"]

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
              | TFfi ("Basis", "string") => false
              | TFfi ("Basis", "blob") => allowHeapAllocated
              | TFfi _ => true
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

fun unurlify env (t, loc) =
    let
        fun unurlify' rf t =
            case t of
                TFfi ("Basis", "unit") => string ("uw_unit_v")
              | TFfi (m, t) => string ("uw_" ^ ident m ^ "_unurlify" ^ capitalize t ^ "(ctx, &request)")

              | TRecord 0 => string "uw_unit_v"
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
                                 string ("] == '/')) ? __uwc_" ^ ident x' ^ "_" ^ Int.toString n),
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
                                  string "3, NULL) : ((!strncmp(request, \"Cons\", 4) && (request[4] == 0 ",
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
                                  string ("(uw_error(ctx, FATAL, \"Error unurlifying list\"), NULL))));"),
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
                     string "? (request += 4, NULL) ",
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

fun urlify env t =
    let
        fun urlify' rf level (t as (_, loc)) =
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
                                                        urlify' rf (level + 1) t,
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
                                  if isUnboxable t then
                                      urlify' rf 0 t
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
                                           urlify' rf 1 t,
                                           string ";",
                                           newline],
                                  string "} else {",
                                  box [string "uw_write(ctx, \"",
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
                                                        urlify' rf 1 t,
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
                              urlify' rf level t]
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
                              urlify' rf (level + 1) t,
                              string ";",
                              newline],
                     string "} else {",
                     box [string "uw_write(ctx, \"None\");",
                          newline],
                     string "}",
                     newline]

              | _ => (ErrorMsg.errorAt loc "Unable to choose a URL encoding function";
                      space)
    in
        urlify' IS.empty 0 t
    end

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
      | EApp ((EError (e, (TFun (_, ran), _)), loc), _) =>
        p_exp env (EError (e, ran), loc)
      | EApp ((EReturnBlob {blob, mimeType, t = (TFun (_, ran), _)}, loc), _) =>
        p_exp env (EReturnBlob {blob = blob, mimeType = mimeType, t = ran}, loc)

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
                 string "uw_error(ctx, FATAL, \"",
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

      | EWrite e => box [string "(uw_write(ctx, ",
                         p_exp env e,
                         string "), uw_unit_v)"]

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
                                    string "__uwr_",
                                    p_ident x,
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

      | EQuery {exps, tables, rnum, state, query, body, initial, prepared} =>
        let
            val exps = map (fn (x, t) => ("__uwf_" ^ ident x, t)) exps
            val tables = ListUtil.mapConcat (fn (x, xts) =>
                                                map (fn (x', t) => ("__uwf_" ^ ident x ^ ".__uwf_" ^ ident x', t)) xts)
                                            tables

            val outputs = exps @ tables
            val outputs = ListMergeSort.sort (fn ((s1, _), (s2, _)) => String.compare (s1, s2) = GREATER) outputs

            val wontLeakStrings = notLeaky env true state
            val wontLeakAnything = notLeaky env false state
        in
            box [if wontLeakAnything then
                     string "(uw_begin_region(ctx), "
                 else
                     box [],
                 string "({",
                 newline,
                 string "PGconn *conn = uw_get_db(ctx);",
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
                 string "int n, i, dummy = (uw_begin_region(ctx), 0);",
                 newline,
                 
                 case prepared of
                     NONE => box [string "char *query = ",
                                  p_exp env query,
                                  string ";",
                                  newline]
                   | SOME _ =>
                     let
                         val ets = getPargs query
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
                                          ets,
                              newline,
                              newline,

                              string "const int paramFormats[] = { ",
                              p_list_sep (box [string ",", space])
                              (fn (_, t) => if isBlob t then string "1" else string "0") ets,
                              string " };",
                              newline,
                              string "const int paramLengths[] = { ",
                              p_list_sepi (box [string ",", space])
                              (fn i => fn (_, Blob) => string ("arg" ^ Int.toString (i + 1) ^ ".size")
                                        | (_, Nullable Blob) => string ("arg" ^ Int.toString (i + 1)
                                                                        ^ "?arg" ^ Int.toString (i + 1) ^ "->size:0")
                                        | _ => string "0") ets,
                              string " };",
                              newline,
                              string "const char *paramValues[] = { ",
                              p_list_sepi (box [string ",", space])
                              (fn i => fn (_, t) => p_ensql t (box [string "arg",
                                                                    string (Int.toString (i + 1))]))
                              ets,
                              string " };",
                              newline,
                              newline]
                     end,

                 string "PGresult *res = ",
                 case prepared of
                     NONE => string "PQexecParams(conn, query, 0, NULL, NULL, NULL, NULL, 0);"
                   | SOME (n, s) =>
                     if #persistent (Settings.currentProtocol ()) then
                         box [string "PQexecPrepared(conn, \"uw",
                              string (Int.toString n),
                              string "\", ",
                              string (Int.toString (length (getPargs query))),
                              string ", paramValues, paramLengths, paramFormats, 0);"]
                     else
                         box [string "PQexecParams(conn, \"",
                              string (String.toString s),
                              string "\", ",
                              string (Int.toString (length (getPargs query))),
                              string ", NULL, paramValues, paramLengths, paramFormats, 0);"],
                 newline,
                 newline,

                 string "if (res == NULL) uw_error(ctx, FATAL, \"Out of memory allocating query result.\");",
                 newline,
                 newline,

                 string "if (PQresultStatus(res) != PGRES_TUPLES_OK) {",
                 newline,
                 box [string "PQclear(res);",
                      newline,
                      string "uw_error(ctx, FATAL, \"",
                      string (ErrorMsg.spanToString loc),
                      string ": Query failed:\\n%s\\n%s\", ",
                      case prepared of
                          NONE => string "query"
                        | SOME _ => p_exp env query,
                      string ", PQerrorMessage(conn));",
                      newline],
                 string "}",
                 newline,
                 newline,

                 string "if (PQnfields(res) != ",
                 string (Int.toString (length outputs)),
                 string ") {",
                 newline,
                 box [string "int nf = PQnfields(res);",
                      newline,
                      string "PQclear(res);",
                      newline,
                      string "uw_error(ctx, FATAL, \"",
                      string (ErrorMsg.spanToString loc),
                      string ": Query returned %d columns instead of ",
                      string (Int.toString (length outputs)),
                      string ":\\n%s\\n%s\", ",
                      case prepared of
                          NONE => string "query"
                        | SOME _ => p_exp env query,
                      string ", nf, PQerrorMessage(conn));",
                      newline],
                 string "}",
                 newline,
                 newline,

                 string "uw_end_region(ctx);",
                 newline,
                 string "uw_push_cleanup(ctx, (void (*)(void *))PQclear, res);",
                 newline,
                 string "n = PQntuples(res);",
                 newline,
                 string "for (i = 0; i < n; ++i) {",
                 newline,
                 box [string "struct",
                      space,
                      string "__uws_",
                      string (Int.toString rnum),
                      space,
                      string "__uwr_r_",
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

                      p_list_sepi (box []) (fn i =>
                                            fn (proj, t) =>
                                               box [string "__uwr_r_",
                                                    string (Int.toString (E.countERels env)),
                                                    string ".",
                                                    string proj,
                                                    space,
                                                    string "=",
                                                    space,
                                                    p_getcol wontLeakStrings env t i,
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
                      newline],
                 string "}",
                 newline,
                 newline,
                 string "uw_pop_cleanup(ctx);",
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

      | EDml {dml, prepared} =>
        box [string "(uw_begin_region(ctx), ({",
             newline,
             string "PGconn *conn = uw_get_db(ctx);",
             newline,
             case prepared of
                 NONE => box [string "char *dml = ",
                              p_exp env dml,
                              string ";",
                              newline]
               | SOME _ =>
                 let
                     val ets = getPargs dml
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
                                      ets,
                          newline,
                          newline,

                          string "const int paramFormats[] = { ",
                          p_list_sep (box [string ",", space])
                                     (fn (_, t) => if isBlob t then string "1" else string "0") ets,
                          string " };",
                          newline,
                          string "const int paramLengths[] = { ",
                          p_list_sepi (box [string ",", space])
                                      (fn i => fn (_, Blob) => string ("arg" ^ Int.toString (i + 1) ^ ".size")
                                                | (_, Nullable Blob) => string ("arg" ^ Int.toString (i + 1)
                                                                                ^ "?arg" ^ Int.toString (i + 1) ^ "->size:0")
                                                | _ => string "0") ets,
                          string " };",
                          newline,
                          string "const char *paramValues[] = { ",
                          p_list_sepi (box [string ",", space])
                                      (fn i => fn (_, t) => p_ensql t (box [string "arg",
                                                                            string (Int.toString (i + 1))]))
                                      ets,
                          string " };",
                          newline,
                          newline]
                 end,
             newline,
             newline,
             string "PGresult *res = ",
             case prepared of
                 NONE => string "PQexecParams(conn, dml, 0, NULL, NULL, NULL, NULL, 0);"
               | SOME (n, s) =>
                 if #persistent (Settings.currentProtocol ()) then
                     box [string "PQexecPrepared(conn, \"uw",
                          string (Int.toString n),
                          string "\", ",
                          string (Int.toString (length (getPargs dml))),
                          string ", paramValues, paramLengths, paramFormats, 0);"]
                 else
                     box [string "PQexecParams(conn, \"",
                          string (String.toString s),
                          string "\", ",
                          string (Int.toString (length (getPargs dml))),
                          string ", NULL, paramValues, paramLengths, paramFormats, 0);"],
             newline,
             newline,

             string "if (res == NULL) uw_error(ctx, FATAL, \"Out of memory allocating DML result.\");",
             newline,
             newline,

             string "if (PQresultStatus(res) != PGRES_COMMAND_OK) {",
             newline,
             box [string "if (!strcmp(PQresultErrorField(res, PG_DIAG_SQLSTATE), \"40001\")) {",
                  box [newline,
                       string "PQclear(res);",
                       newline,
                       string "uw_error(ctx, UNLIMITED_RETRY, \"Serialization failure\");",
                       newline],
                  string "}",
                  newline,
                  string "PQclear(res);",
                  newline,
                  string "uw_error(ctx, FATAL, \"",
                  string (ErrorMsg.spanToString loc),
                  string ": DML failed:\\n%s\\n%s\", ",
                  case prepared of
                      NONE => string "dml"
                    | SOME _ => p_exp env dml,
                  string ", PQerrorMessage(conn));",
                  newline],
             string "}",
             newline,
             newline,

             string "PQclear(res);",
             newline,
             string "uw_end_region(ctx);",
             newline,
             string "uw_unit_v;",
             newline,
             string "}))"]

      | ENextval {seq, prepared} =>
        let
            val query = case seq of
                            (EPrim (Prim.String s), loc) =>
                            (EPrim (Prim.String ("SELECT NEXTVAL('" ^ s ^ "')")), loc)
                          | _ =>
                            let
                                val query = (EFfiApp ("Basis", "strcat", [seq, (EPrim (Prim.String "')"), loc)]), loc)
                            in
                                (EFfiApp ("Basis", "strcat", [(EPrim (Prim.String "SELECT NEXTVAL('"), loc), query]), loc)
                            end
        in
            box [string "(uw_begin_region(ctx), ",
                 string "({",
                 newline,
                 string "PGconn *conn = uw_get_db(ctx);",
                 newline,
                 case prepared of
                     NONE => box [string "char *query = ",
                                  p_exp env query,
                                  string ";",
                                  newline]
                   | SOME _ =>
                     box [],
                 newline,
                 string "PGresult *res = ",
                 case prepared of
                     NONE => string "PQexecParams(conn, query, 0, NULL, NULL, NULL, NULL, 0);"
                   | SOME (n, s) =>
                     if #persistent (Settings.currentProtocol ()) then
                         box [string "PQexecPrepared(conn, \"uw",
                              string (Int.toString n),
                              string "\", 0, NULL, NULL, NULL, 0);"]
                     else
                         box [string "PQexecParams(conn, \"uw",
                              string (Int.toString n),
                              string "\", 0, NULL, NULL, NULL, NULL, 0);"],
                 newline,
                 string "uw_Basis_int n;",
                 newline,
                 newline,

                 string "if (res == NULL) uw_error(ctx, FATAL, \"Out of memory allocating nextval result.\");",
                 newline,
                 newline,

                 string "if (PQresultStatus(res) != PGRES_TUPLES_OK) {",
                 newline,
                 box [string "PQclear(res);",
                      newline,
                      string "uw_error(ctx, FATAL, \"",
                      string (ErrorMsg.spanToString loc),
                      string ": Query failed:\\n%s\\n%s\", ",
                      case prepared of
                          NONE => string "query"
                        | SOME _ => p_exp env query,
                      string ", PQerrorMessage(conn));",
                      newline],
                 string "}",
                 newline,
                 newline,

                 string "uw_end_region(ctx);",
                 newline,
                 string "n = PQntuples(res);",
                 newline,
                 string "if (n != 1) {",
                 newline,
                 box [string "PQclear(res);",
                      newline,
                      string "uw_error(ctx, FATAL, \"",
                      string (ErrorMsg.spanToString loc),
                      string ": Wrong number of result rows:\\n%s\\n%s\", ",
                      case prepared of
                          NONE => string "query"
                        | SOME _ => p_exp env query,
                      string ", PQerrorMessage(conn));",
                      newline],
                 string "}",
                 newline,
                 newline,

                 string "n = ",
                 p_unsql true env (TFfi ("Basis", "int"), loc)
                         (string "PQgetvalue(res, 0, 0)")
                         (box []),
                 string ";",
                 newline,
                 string "PQclear(res);",
                 newline,
                 string "n;",
                 newline,
                 string "}))"]
        end

      | EUnurlify (e, t) =>
        let
            fun getIt () =
                if isUnboxable t then
                    unurlify env t
                else
                    box [string "({",
                         newline,
                         p_typ env t,
                         string " *tmp = uw_malloc(ctx, sizeof(",
                         p_typ env t,
                         string "));",
                         newline,
                         string "*tmp = ",
                         unurlify env t,
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
                                             string ("__uwn_" ^ ident fx ^ "_" ^ Int.toString n),
                                             string "(uw_context,",
                                             space,
                                             p_list_sep (box [string ",", space])
                                                        (fn (_, dom) => p_typ env dom) args,
                                             string ");"]) vis,
                 newline,
                 p_list_sep newline (p_fun env) vis,
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
      | DDatabase {name, expunge, initialize} =>
        box [string "static void uw_db_validate(uw_context);",
             newline,
             string "static void uw_db_prepare(uw_context);",
             newline,
             newline,
             string "void uw_db_init(uw_context ctx) {",
             newline,
             string "PGconn *conn = PQconnectdb(\"",
             string (String.toString name),
             string "\");",
             newline,
             string "if (conn == NULL) uw_error(ctx, BOUNDED_RETRY, ",
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
             newline,

             string "void uw_expunger(uw_context ctx, uw_Basis_client cli) {",
             newline,
             box [p_enamed env expunge,
                  string "(ctx, cli);",
                  newline],
             string "}",
             newline,
             newline,

             string "void uw_initializer(uw_context ctx) {",
             newline,
             box [p_enamed env initialize,
                  string "(ctx, uw_unit_v);",
                  newline],
             string "}",
             newline]

      | DPreparedStatements [] =>
        box [string "static void uw_db_prepare(uw_context ctx) {",
             newline,
             string "}"]
      | DPreparedStatements ss =>
        if #persistent (Settings.currentProtocol ()) then
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
                 
                 string "}"]
        else
            string "static void uw_db_prepare(uw_context ctx) { }"

      | DJavaScript s => box [string "static char jslib[] = \"",
                              string (String.toString s),
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

        val fields = foldl (fn ((ek, _, _, ts, _, _), fields) =>
                               case ek of
                                   Link => fields
                                 | Rpc _ => fields
                                 | Action eff =>
                                   case List.nth (ts, length ts - 2) of
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
                              unurlify env t,
                              string ";",
                              newline]
            end

        fun p_page (ek, s, n, ts, ran, side) =
            let
                val (ts, defInputs, inputsVar, fields) =
                    case ek of
                        Core.Link => (List.take (ts, length ts - 1), string "", string "", NONE)
                      | Core.Rpc _ => (List.take (ts, length ts - 1), string "", string "", NONE)
                      | Core.Action _ =>
                        case List.nth (ts, length ts - 2) of
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
                                      string "struct __uws_",
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
                                      string "};",
                                      newline],
                                 box [string ",",
                                      space,
                                      string "uw_inputs"],
                                 SOME xts)
                            end

                          | _ => raise Fail "CjrPrint: Last argument to an action isn't a record"

                fun couldWrite ek =
                    case ek of
                        Link => false
                      | Action ef => ef = ReadCookieWrite
                      | Rpc ef => ef = ReadCookieWrite
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
                     if couldWrite ek then
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
                                    string "uw_set_script_header(ctx, \"",
                                    let
                                        val scripts =
                                            case side of
                                                ServerOnly => ""
                                              | _ => "<script src=\\\""
                                                     ^ OS.Path.joinDirFile {dir = Settings.getUrlPrefix (),
                                                                            file = "app.js"}
                                                     ^ "\\\"></script>\\n"

                                        val scripts = foldl (fn (x, scripts) =>
                                                                scripts
                                                                ^ "<script src=\\\"" ^ x ^ "\\\"></script>\\n")
                                                      scripts (Settings.getScripts ())
                                    in
                                        string scripts
                                    end,
                                    string "\");",
                                    newline,
                                    string "uw_set_needs_push(ctx, ",
                                    string (case side of
                                                ServerAndPullAndPush => "1"
                                              | _ => "0"),
                                    string ");",
                                    newline,
                                    string "uw_set_url_prefix(ctx, \"",
                                    string (Settings.getUrlPrefix ()),
                                    string "\");",
                                    newline]),
                     string "uw_set_needs_sig(ctx, ",
                     string (if couldWrite ek then
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
                                                                unurlify env t,
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
                          string ", uw_unit_v);",
                          newline,
                          box (case ek of
                                   Core.Rpc _ => [urlify env ran]
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

        val tables = List.mapPartial (fn (DTable (s, xts, _, _), _) => SOME (s, xts)
                                       | _ => NONE) ds
        val sequences = List.mapPartial (fn (DSequence s, _) => SOME s
                                          | _ => NONE) ds

        val validate =
            if #persistent (Settings.currentProtocol ()) then
                box [string "static void uw_db_validate(uw_context ctx) {",
                     newline,
                     string "PGconn *conn = uw_get_db(ctx);",
                     newline,
                     string "PGresult *res;",
                     newline,
                     newline,
                     p_list_sep newline
                                (fn (s, xts) =>
                                    let
                                        val sl = CharVector.map Char.toLower s

                                        val q = "SELECT COUNT(*) FROM pg_class WHERE relname = '"
                                                ^ sl ^ "'"

                                        val q' = String.concat ["SELECT COUNT(*) FROM pg_attribute WHERE attrelid = (SELECT oid FROM pg_class WHERE relname = '",
                                                                sl,
                                                                "') AND (",
                                                                String.concatWith " OR "
                                                                                  (map (fn (x, t) =>
                                                                                           String.concat ["(attname = 'uw_",
                                                                                                          CharVector.map
                                                                                                              Char.toLower (ident x),
                                                                                                          "' AND atttypid = (SELECT oid FROM pg_type",
                                                                                                          " WHERE typname = '",
                                                                                                          p_sqltype_base' env t,
                                                                                                          "') AND attnotnull = ",
                                                                                                          if is_not_null t then
                                                                                                              "TRUE"
                                                                                                          else
                                                                                                              "FALSE",
                                                                                                          ")"]) xts),
                                                                ")"]

                                        val q'' = String.concat ["SELECT COUNT(*) FROM pg_attribute WHERE attrelid = (SELECT oid FROM pg_class WHERE relname = '",
                                                                 sl,
                                                                 "') AND attname LIKE 'uw_%'"]
                                    in
                                        box [string "res = PQexec(conn, \"",
                                             string q,
                                             string "\");",
                                             newline,
                                             newline,
                                             string "if (res == NULL) {",
                                             newline,
                                             box [string "PQfinish(conn);",
                                                  newline,
                                                  string "uw_error(ctx, FATAL, \"Out of memory allocating query result.\");",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "if (PQresultStatus(res) != PGRES_TUPLES_OK) {",
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
                                                  string "uw_error(ctx, FATAL, \"Query failed:\\n",
                                                  string q,
                                                  string "\\n%s\", msg);",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "if (strcmp(PQgetvalue(res, 0, 0), \"1\")) {",
                                             newline,
                                             box [string "PQclear(res);",
                                                  newline,
                                                  string "PQfinish(conn);",
                                                  newline,
                                                  string "uw_error(ctx, FATAL, \"Table '",
                                                  string s,
                                                  string "' does not exist.\");",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "PQclear(res);",
                                             newline,

                                             string "res = PQexec(conn, \"",
                                             string q',
                                             string "\");",
                                             newline,
                                             newline,
                                             string "if (res == NULL) {",
                                             newline,
                                             box [string "PQfinish(conn);",
                                                  newline,
                                                  string "uw_error(ctx, FATAL, \"Out of memory allocating query result.\");",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "if (PQresultStatus(res) != PGRES_TUPLES_OK) {",
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
                                                  string "uw_error(ctx, FATAL, \"Query failed:\\n",
                                                  string q',
                                                  string "\\n%s\", msg);",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "if (strcmp(PQgetvalue(res, 0, 0), \"",
                                             string (Int.toString (length xts)),
                                             string "\")) {",
                                             newline,
                                             box [string "PQclear(res);",
                                                  newline,
                                                  string "PQfinish(conn);",
                                                  newline,
                                                  string "uw_error(ctx, FATAL, \"Table '",
                                                  string s,
                                                  string "' has the wrong column types.\");",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "PQclear(res);",
                                             newline,
                                             newline,

                                             string "res = PQexec(conn, \"",
                                             string q'',
                                             string "\");",
                                             newline,
                                             newline,
                                             string "if (res == NULL) {",
                                             newline,
                                             box [string "PQfinish(conn);",
                                                  newline,
                                                  string "uw_error(ctx, FATAL, \"Out of memory allocating query result.\");",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "if (PQresultStatus(res) != PGRES_TUPLES_OK) {",
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
                                                  string "uw_error(ctx, FATAL, \"Query failed:\\n",
                                                  string q'',
                                                  string "\\n%s\", msg);",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "if (strcmp(PQgetvalue(res, 0, 0), \"",
                                             string (Int.toString (length xts)),
                                             string "\")) {",
                                             newline,
                                             box [string "PQclear(res);",
                                                  newline,
                                                  string "PQfinish(conn);",
                                                  newline,
                                                  string "uw_error(ctx, FATAL, \"Table '",
                                                  string s,
                                                  string "' has extra columns.\");",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "PQclear(res);",
                                             newline]
                                    end) tables,

                     p_list_sep newline
                                (fn s =>
                                    let
                                        val sl = CharVector.map Char.toLower s

                                        val q = "SELECT COUNT(*) FROM pg_class WHERE relname = '"
                                                ^ sl ^ "' AND relkind = 'S'"
                                    in
                                        box [string "res = PQexec(conn, \"",
                                             string q,
                                             string "\");",
                                             newline,
                                             newline,
                                             string "if (res == NULL) {",
                                             newline,
                                             box [string "PQfinish(conn);",
                                                  newline,
                                                  string "uw_error(ctx, FATAL, \"Out of memory allocating query result.\");",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "if (PQresultStatus(res) != PGRES_TUPLES_OK) {",
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
                                                  string "uw_error(ctx, FATAL, \"Query failed:\\n",
                                                  string q,
                                                  string "\\n%s\", msg);",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "if (strcmp(PQgetvalue(res, 0, 0), \"1\")) {",
                                             newline,
                                             box [string "PQclear(res);",
                                                  newline,
                                                  string "PQfinish(conn);",
                                                  newline,
                                                  string "uw_error(ctx, FATAL, \"Sequence '",
                                                  string s,
                                                  string "' does not exist.\");",
                                                  newline],
                                             string "}",
                                             newline,
                                             newline,
                                             string "PQclear(res);",
                                             newline]
                                    end) sequences,

                     string "}"]
            else
                string "static void uw_db_validate(uw_context ctx) { }"

        val hasDb = List.exists (fn (DDatabase _, _) => true | _ => false) ds

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
            box [string "int ",
                 string name,
                 string "(const char *s) {",
                 newline,
                 box [p_list_sep (box [])
                      (fn rule =>
                          box [string "if (!str",
                               case #kind rule of
                                   Settings.Exact => box [string "cmp(s, \"",
                                                          string (String.toString (#pattern rule)),
                                                          string "\"))"]
                                 | Settings.Prefix => box [string "ncmp(s, \"",
                                                           string (String.toString (#pattern rule)),
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
    in
        box [string "#include <stdio.h>",
             newline,
             string "#include <stdlib.h>",
             newline,
             string "#include <string.h>",
             newline,
             string "#include <math.h>",
             newline,
             if hasDb then
                 box [string "#include <postgresql/libpq-fe.h>",
                      newline]
             else
                 box [],
             newline,
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

             string "static const char begin_xhtml[] = \"<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\" ?>\\n<!DOCTYPE html PUBLIC \\\"-//W3C//DTD XHTML 1.0 Transitional//EN\\\" \\\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\\\">\\n<html xmlns=\\\"http://www.w3.org/1999/xhtml\\\" xml:lang=\\\"en\\\" lang=\\\"en\\\">\";",
             newline,
             newline,

             p_list_sep newline (fn x => x) pds,
             newline,
             string "int uw_inputs_len = ",
             string (Int.toString (SM.foldl Int.max 0 fnums + 1)),
             string ";",
             newline,
             string "int uw_timeout = ",
             string (Int.toString (Settings.getTimeout ())),
             string ";",
             newline,
             newline,
             string "int uw_input_num(char *name) {",
             newline,
             makeSwitch (fnums, 0),
             string "}",
             newline,
             newline,

             makeChecker ("uw_check_url", Settings.getUrlRules ()),
             newline,

             makeChecker ("uw_check_mime", Settings.getMimeRules ()),
             newline,
             
             string "extern void uw_sign(const char *in, char *out);",
             newline,
             string "extern int uw_hash_blocksize;",
             newline,
             string "uw_Basis_string uw_cookie_sig(uw_context ctx) {",
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

             string "void uw_handle(uw_context ctx, char *request) {",
             newline,
             string "if (!strcmp(request, \"/app.js\")) {",
             newline,
             box [string "uw_write_header(ctx, \"Content-type: text/javascript\\r\\n\");",
                  newline,
                  string "uw_write(ctx, jslib);",
                  newline,
                  string "return;",
                  newline],
             string "}",
             newline,
             p_list_sep newline (fn x => x) pds',
             newline,
             string "uw_error(ctx, FATAL, \"Unknown page\");",
             newline,
             string "}",
             newline,
             newline,
             if hasDb then
                 validate
             else
                 box [],
             newline,
             if List.exists (fn (DDatabase _, _) => true | _ => false) ds then
                 box []
             else
                 box [newline,
                      string "void uw_db_init(uw_context ctx) { };",
                      newline,
                      string "int uw_db_begin(uw_context ctx) { return 0; };",
                      newline,
                      string "int uw_db_commit(uw_context ctx) { return 0; };",
                      newline,
                      string "int uw_db_rollback(uw_context ctx) { return 0; };",
                      newline,
                      string "void uw_expunger(uw_context ctx, uw_Basis_client cli) { };",
                      newline,
                      string "void uw_initializer(uw_context ctx) { };",
                      newline]]
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
                                                            box [string "uw_",
                                                                 string (CharVector.map Char.toLower x),
                                                                 space,
                                                                 p_sqltype env (t, ErrorMsg.dummySpan)]) xts,
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
                                            box [string "CREATE SEQUENCE ",
                                                 string s,
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
        box pps
    end

end
