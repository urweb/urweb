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

structure E = CjrEnv
structure EM = ErrorMsg

val debug = ref false

val dummyTyp = (TNamed 0, ErrorMsg.dummySpan)

fun p_typ' par env (t, loc) =
    case t of
        TTop => string "void*"
      | TFun (t1, t2) => parenIf par (box [p_typ' true env t2,
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
      | TNamed n =>
        (string ("__lwt_" ^ #1 (E.lookupTNamed env n) ^ "_" ^ Int.toString n)
         handle CjrEnv.UnboundNamed _ => string ("__lwt_UNBOUND__" ^ Int.toString n))
      | TFfi (m, x) => box [string "lw_", string m, string "_", string x]

and p_typ env = p_typ' false env

fun p_rel env n = string ("__lwr_" ^ #1 (E.lookupERel env n) ^ "_" ^ Int.toString (E.countERels env - n - 1))
    handle CjrEnv.UnboundRel _ => string ("__lwr_UNBOUND_" ^ Int.toString (E.countERels env - n - 1))

fun p_enamed env n =
    string ("__lwn_" ^ #1 (E.lookupENamed env n) ^ "_" ^ Int.toString n)
    handle CjrEnv.UnboundNamed _ => string ("__lwn_UNBOUND_" ^ Int.toString n)

fun p_exp' par env (e, _) =
    case e of
        EPrim p => Prim.p_t p
      | ERel n => p_rel env n
      | ENamed n => p_enamed env n

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
                                 string "__lw_tmp",
                                 space,
                                 string "=",
                                 space,
                                 string "{",
                                 p_list (fn (_, e) =>
                                            p_exp env e) xes,
                                 string "};",
                                 space,
                                 string "__lw_tmp;",
                                 space,
                                 string "})" ]
      | EField (e, x) =>
        box [p_exp' true env e,
             string ".",
             string x]

      | EWrite e => box [string "(lw_write(ctx, ",
                         p_exp env e,
                         string "), lw_unit_v)"]

      | ESeq (e1, e2) => box [string "(",
                              p_exp env e1,
                              string ",",
                              space,
                              p_exp env e2,
                              string ")"]

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
        box [string "struct",
             space,
             string ("__lws_" ^ Int.toString n),
             space,
             string "{",
             newline,
             p_list_sep (box []) (fn (x, t) => box [p_typ env t,
                                                    space,
                                                    string x,
                                                    string ";",
                                                    newline]) xts,
             string "};"]

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

fun unurlify (t, loc) =
    case t of
        TFfi ("Basis", "int") => string "lw_unurlifyInt(&request)"
      | TFfi ("Basis", "float") => string "lw_unurlifyFloat(&request)"
      | TFfi ("Basis", "string") => string "lw_unurlifyString(ctx, &request)"

      | TRecord 0 => string "lw_unit_v"

      | _ => (ErrorMsg.errorAt loc "Unable to choose a URL decoding function";
              space)

fun p_page env (s, n, ts) =
    box [string "if (!strncmp(request, \"",
         string (String.toString s),
         string "\", ",
         string (Int.toString (size s)),
         string ")) {",
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
              p_enamed env n,
              string "(",
              p_list_sep (box [string ",", space])
                         (fn x => x)
                         (string "ctx" :: ListUtil.mapi (fn (i, _) => string ("arg" ^ Int.toString i)) ts),
              string ");",
              newline,
              string "return;",
              newline,
              string "}",
              newline,
              string "}"]
        ]

fun p_file env (ds, ps) =
    let
        val (pds, env) = ListUtil.foldlMap (fn (d, env) =>
                                             (p_decl env d,
                                              E.declBinds env d))
                             env ds
        val pds' = map (p_page env) ps
    in
        box [string "#include \"lacweb.h\"",
             newline,
             newline,
             p_list_sep newline (fn x => x) pds,
             newline,
             string "void lw_handle(lw_context ctx, char *request) {",
             newline,
             p_list_sep newline (fn x => x) pds',
             newline,
             string "}",
             newline]
    end

end
