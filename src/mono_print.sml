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

(* Pretty-printing monomorphic Ur/Web *)

structure MonoPrint :> MONO_PRINT = struct

open Print.PD
open Print

open Mono

structure E = MonoEnv

val debug = ref false

val dummyt = (TRecord [], ErrorMsg.dummySpan)

fun p_typ' par env (t, _) =
    case t of
        TFun (t1, t2) => parenIf par (box [p_typ' true env t1,
                                           space,
                                           string "->",
                                           space,
                                           p_typ env t2])
      | TRecord xcs => box [string "{",
                            p_list (fn (x, t) =>
                                       box [string x,
                                            space,
                                            string ":",
                                            space,
                                            p_typ env t]) xcs,
                            string "}"]
      | TDatatype (n, _) =>
        ((if !debug then
              string (#1 (E.lookupDatatype env n) ^ "__" ^ Int.toString n)
          else
              string (#1 (E.lookupDatatype env n)))
         handle E.UnboundNamed _ => string ("UNBOUND_DATATYPE_" ^ Int.toString n))
      | TFfi (m, x) => box [string "FFI(", string m, string ".", string x, string ")"]
      | TOption t => box [string "option(",
                          p_typ env t,
                          string ")"]
      | TSource => string "source"
      | TSignal t => box [string "signal(",
                          p_typ env t,
                          string ")"]

and p_typ env = p_typ' false env

fun p_enamed env n =
    (if !debug then
         string (#1 (E.lookupENamed env n) ^ "__" ^ Int.toString n)
     else
         string (#1 (E.lookupENamed env n)))
    handle E.UnboundNamed _ => string ("UNBOUNDN_" ^ Int.toString n)

fun p_con_named env n =
    (if !debug then
        string (#1 (E.lookupConstructor env n) ^ "__" ^ Int.toString n)
     else
         string (#1 (E.lookupConstructor env n)))
    handle E.UnboundNamed _ => string ("CONSTRUCTOR_" ^ Int.toString n)

fun p_patCon env pc =
    case pc of
        PConVar n => p_con_named env n
      | PConFfi {mod = m, con, ...} => box [string "FFIC(",
                                            string m,
                                            string ".",
                                            string con,
                                            string ")"]

fun p_pat' par env (p, _) =
    case p of
        PWild => string "_"
      | PVar (s, _) => string s
      | PPrim p => Prim.p_t p
      | PCon (_, n, NONE) => p_patCon env n
      | PCon (_, n, SOME p) => parenIf par (box [p_patCon env n,
                                                 space,
                                                 p_pat' true env p])
      | PRecord xps =>
        box [string "{",
             p_list_sep (box [string ",", space]) (fn (x, p, _) =>
                                                      box [string x,
                                                           space,
                                                           string "=",
                                                           space,
                                                           p_pat env p]) xps,
             string "}"]
      | PNone _ => string "None"
      | PSome (_, p) => box [string "Some",
                             space,
                             p_pat' true env p]

and p_pat x = p_pat' false x

fun p_mode env m =
    case m of
        Attribute => string "Attribute"
      | Script => string "Script"
      | Source t => box [string "Source", space, p_typ env t]

fun p_exp' par env (e, _) =
    case e of
        EPrim p => Prim.p_t p
      | ERel n =>
        ((if !debug then
              string (#1 (E.lookupERel env n) ^ "_" ^ Int.toString n)
          else
              string (#1 (E.lookupERel env n)))
         handle E.UnboundRel _ => string ("UNBOUND_" ^ Int.toString n))
      | ENamed n => p_enamed env n
      | ECon (_, pc, NONE) => p_patCon env pc
      | ECon (_, pc, SOME e) => parenIf par (box [p_patCon env pc,
                                                  space,
                                                  p_exp' true env e])
      | ENone _ => string "None"
      | ESome (_, e) => parenIf par (box [string "Some",
                                          space,
                                          p_exp' true env e])

      | EFfi (m, x) => box [string "FFI(", string m, string ".", string x, string ")"]
      | EFfiApp (m, x, es) => box [string "FFI(",
                                   string m,
                                   string ".",
                                   string x,
                                   string "(",
                                   p_list (p_exp env) es,
                                   string "))"]
      | EApp (e1, e2) => parenIf par (box [p_exp env e1,
                                           space,
                                           p_exp' true env e2])
      | EAbs (x, t, _, e) => parenIf true (box [string "fn",
                                                space,
                                                string x,
                                                space,
                                                string ":",
                                                space,
                                                p_typ env t,
                                                space,
                                                string "=>",
                                                space,
                                                p_exp (E.pushERel env x t NONE) e])

      | EUnop (s, e) => parenIf true (box [string s,
                                           space,
                                           p_exp' true env e])
      | EBinop (s, e1, e2) => parenIf true (box [p_exp' true env e1,
                                                 space,
                                                 string s,
                                                 space,
                                                 p_exp' true env e2])

      | ERecord xes => box [string "{",
                            p_list (fn (x, e, _) =>
                                       box [string x,
                                            space,
                                            string "=",
                                            space,
                                            p_exp env e]) xes,
                            string "}"]
      | EField (e, x) =>
        box [p_exp' true env e,
             string ".",
             string x]

      | ECase (e, pes, _) => parenIf true (box [string "case",
                                                space,
                                                p_exp env e,
                                                space,
                                                string "of",
                                                space,
                                                p_list_sep (box [space, string "|", space])
                                                           (fn (p, e) => box [p_pat env p,
                                                                              space,
                                                                              string "=>",
                                                                              space,
                                                                              p_exp (E.patBinds env p) e]) pes])

      | EError (e, t) => box [string "(error",
                              space,
                              p_exp env e,
                              space,
                              string ":",
                              space,
                              p_typ env t,
                              string ")"]

      | EStrcat (e1, e2) => parenIf par (box [p_exp' true env e1,
                                              space,
                                              string "^",
                                              space,
                                              p_exp env e2])

      | EWrite e => box [string "write(",
                         p_exp env e,
                         string ")"]

      | ESeq (e1, e2) => box [string "(",
                              p_exp env e1,
                              string ";",
                              space,
                              p_exp env e2,
                              string ")"]
      | ELet (x, t, e1, e2) => box [string "(let",
                                    space,
                                    string x,
                                    space,
                                    string ":",
                                    space,
                                    p_typ env t,
                                    space,
                                    string "=",
                                    space,
                                    string "(",
                                    p_exp env e1,
                                    string ")",
                                    space,
                                    string "in",
                                    space,
                                    string "(",
                                    p_exp (E.pushERel env x t NONE) e2,
                                    string "))"]

      | EClosure (n, es) => box [string "CLOSURE(",
                                 p_enamed env n,
                                 p_list_sep (string "") (fn e => box [string ", ",
                                                                      p_exp env e]) es,
                                 string ")"]

      | EQuery {exps, tables, state, query, body, initial} =>
        box [string "query[",
             p_list (fn (x, t) => box [string x, space, string ":", space, p_typ env t]) exps,
             string "] [",
             p_list (fn (x, xts) => box [string x,
                                         space,
                                         string ":",
                                         space,
                                         string "{",
                                         p_list (fn (x, t) => box [string x, space, string ":", space, p_typ env t]) xts,
                                         string "}"]) tables,
             string "] [",
             p_typ env state,
             string "]",
             space,
             p_exp env query,
             space,
             string "initial",
             space,
             p_exp env initial,
             space,
             string "in",
             space,
             p_exp (E.pushERel (E.pushERel env "r" dummyt NONE) "acc" dummyt NONE) body]
      | EDml e => box [string "dml(",
                       p_exp env e,
                       string ")"]
      | ENextval e => box [string "nextval(",
                           p_exp env e,
                           string ")"]
      | EUnurlify (e, _) => box [string "unurlify(",
                                 p_exp env e,
                                 string ")"]
      | EJavaScript (m, e, NONE) => box [string "JavaScript(",
                                         p_mode env m,
                                         string ",",
                                         space,
                                         p_exp env e,
                                         string ")"]
      | EJavaScript (_, _, SOME e) => p_exp env e

      | ESignalReturn e => box [string "Return(",
                                p_exp env e,
                                string ")"]
      | ESignalBind (e1, e2) => box [string "Bind(",
                                     p_exp env e1,
                                     string ",",
                                     space,
                                     p_exp env e2,
                                     string ")"]
      | ESignalSource e => box [string "Source(",
                                p_exp env e,
                                string ")"]

      | EServerCall (n, es, e) => box [string "Server(",
                                       p_enamed env n,
                                       string ",",
                                       space,
                                       p_list (p_exp env) es,
                                       string ")[",
                                       p_exp env e,
                                       string "]"]

and p_exp env = p_exp' false env

fun p_vali env (x, n, t, e, s) =
    let
        val xp = if !debug then
                     box [string x,
                          string "__",
                          string (Int.toString n)]
                 else
                     string x        
    in
        box [xp,
             space,
             string "as",
             space,
             string s,
             space,
             string ":",
             space,
             p_typ env t,
             space,
             string "=",
             space,
             p_exp env e]
    end

fun p_datatype env (x, n, cons) =
    let
        val env = E.pushDatatype env x n cons
    in
        box [string "datatype",
             space,
             string x,
             space,
             string "=",
             space,
             p_list_sep (box [space, string "|", space])
                        (fn (x, n, NONE) => if !debug then (string (x ^ "__" ^ Int.toString n))
                                            else string x
                          | (x, _, SOME t) => box [if !debug then (string (x ^ "__" ^ Int.toString n))
                                                   else string x, space, string "of", space, p_typ env t])
                        cons]
    end

fun p_decl env (dAll as (d, _) : decl) =
    case d of
        DDatatype x => p_datatype env x
      | DVal vi => box [string "val",
                        space,
                        p_vali env vi]
      | DValRec vis =>
        let
            val env = E.declBinds env dAll
        in
            box [string "val",
                 space,
                 string "rec",
                 space,
                 p_list_sep (box [newline, string "and", space]) (p_vali env) vis]
        end

      | DExport (ek, s, n, ts) => box [string "export",
                                       space,
                                       CorePrint.p_export_kind ek,
                                       space,
                                       p_enamed env n,
                                       space,
                                       string "as",
                                       space,
                                       string s,
                                       p_list_sep (string "") (fn t => box [space,
                                                                            string "(",
                                                                            p_typ env t,
                                                                            string ")"]) ts]

      | DTable (s, xts) => box [string "(* SQL table ",
                                string s,
                                space,
                                string ":",
                                space,
                                p_list (fn (x, t) => box [string x,
                                                          space,
                                                          string ":",
                                                          space,
                                                          p_typ env t]) xts,
                                space,
                                string "*)"]
      | DSequence s => box [string "(* SQL sequence ",
                            string s,
                            string "*)"]
      | DDatabase s => box [string "database",
                            space,
                            string s]
      | DJavaScript s => box [string "JavaScript(",
                              string s,
                              string ")"]

                          
fun p_file env file =
    let
        val (pds, _) = ListUtil.foldlMap (fn (d, env) =>
                                             (p_decl env d,
                                              E.declBinds env d))
                             env file
    in
        p_list_sep newline (fn x => x) pds
    end

end
