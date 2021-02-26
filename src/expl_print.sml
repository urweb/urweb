(* Copyright (c) 2008-2010, Adam Chlipala
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

(* Pretty-printing elaborated Ur/Web *)

structure ExplPrint :> EXPL_PRINT = struct

open Print.PD
open Print

open Expl

structure E = ExplEnv

val debug = ref false

fun p_kind' par env (k, _) =
    case k of
        KType => string "Type"
      | KArrow (k1, k2) => parenIf par (box [p_kind' true env k1,
                                             space,
                                             string "->",
                                             space,
                                             p_kind env k2])
      | KName => string "Name"
      | KRecord k => box [string "{", p_kind env k, string "}"]
      | KUnit => string "Unit"
      | KTuple ks => box [string "(",
                          p_list_sep (box [space, string "*", space]) (p_kind env) ks,
                          string ")"]

      | KRel n => ((if !debug then
                         string (E.lookupKRel env n ^ "_" ^ Int.toString n)
                     else
                         string (E.lookupKRel env n))
                    handle E.UnboundRel _ => string ("UNBOUND_REL" ^ Int.toString n))
      | KFun (x, k) => box [string x,
                            space,
                            string "-->",
                            space,
                            p_kind (E.pushKRel env x) k]

and p_kind env = p_kind' false env

fun p_con' par env (c, _) =
    case c of
        TFun (t1, t2) => parenIf par (box [p_con' true env t1,
                                           space,
                                           string "->",
                                           space,
                                           p_con env t2])
      | TCFun (x, k, c) => parenIf par (box [string x,
                                             space,
                                             string "::",
                                             space,
                                             p_kind env k,
                                             space,
                                             string "->",
                                             space,
                                                p_con (E.pushCRel env x k) c])
      | TRecord (CRecord (_, xcs), _) => box [string "{",
                                              p_list (fn (x, c) =>
                                                         box [p_name env x,
                                                              space,
                                                              string ":",
                                                              space,
                                                              p_con env c]) xcs,
                                              string "}"]
      | TRecord c => box [string "$",
                          p_con' true env c]

      | CRel n =>
        ((if !debug then
              string (#1 (E.lookupCRel env n) ^ "_" ^ Int.toString n)
          else
              string (#1 (E.lookupCRel env n)))
         handle E.UnboundRel _ => string ("UNBOUND_REL" ^ Int.toString n))
      | CNamed n =>
        ((if !debug then
              string (#1 (E.lookupCNamed env n) ^ "__" ^ Int.toString n)
          else
              string (#1 (E.lookupCNamed env n)))
         handle E.UnboundNamed _ => string ("UNBOUND_NAMED" ^ Int.toString n))
      | CModProj (m1, ms, x) =>
        let
            val m1x = #1 (E.lookupStrNamed env m1)
                      handle E.UnboundNamed _ => "UNBOUND" ^ Int.toString m1

            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
        in
            p_list_sep (string ".") string (m1s :: ms @ [x])
        end

      | CApp (c1, c2) => parenIf par (box [p_con env c1,
                                           space,
                                           p_con' true env c2])
      | CAbs (x, k, c) => parenIf par (box [string "fn",
                                            space,
                                            string x,
                                            space,
                                            string "::",
                                            space,
                                            p_kind env k,
                                            space,
                                            string "=>",
                                            space,
                                            p_con (E.pushCRel env x k) c])

      | CName s => box [string "#", string s]

      | CRecord (k, xcs) =>
        if !debug then
            parenIf par (box [string "[",
                              p_list (fn (x, c) =>
                                         box [p_con env x,
                                              space,
                                              string "=",
                                              space,
                                              p_con env c]) xcs,
                              string "]::",
                              p_kind env k])
        else
            parenIf par (box [string "[",
                              p_list (fn (x, c) =>
                                         box [p_con env x,
                                              space,
                                              string "=",
                                              space,
                                              p_con env c]) xcs,
                              string "]"])
      | CConcat (c1, c2) => parenIf par (box [p_con' true env c1,
                                              space,
                                              string "++",
                                              space,
                                              p_con env c2])
      | CMap _ => string "map"
      | CUnit => string "()"

      | CTuple cs => box [string "(",
                          p_list (p_con env) cs,
                          string ")"]
      | CProj (c, n) => box [p_con env c,
                             string ".",
                             string (Int.toString n)]

      | CKAbs (x, c) => box [string x,
                             space,
                             string "==>",
                             space,
                             p_con (E.pushKRel env x) c]
      | CKApp (c, k) => box [p_con env c,
                             string "[[",
                             p_kind env k,
                             string "]]"]
      | TKFun (x, c) => box [string x,
                             space,
                             string "-->",
                             space,
                             p_con (E.pushKRel env x) c]
        
and p_con env = p_con' false env

and p_name env (all as (c, _)) =
    case c of
        CName s => string s
      | _ => p_con env all

fun p_patCon env pc =
    case pc of
        PConVar n =>
        ((if !debug then
              string (#1 (E.lookupENamed env n) ^ "__" ^ Int.toString n)
          else
              string (#1 (E.lookupENamed env n)))
         handle E.UnboundNamed _ => string ("UNBOUND_NAMED" ^ Int.toString n))
      | PConProj (m1, ms, x) =>
        let
            val m1x = #1 (E.lookupStrNamed env m1)
                handle E.UnboundNamed _ => "UNBOUND_STR_" ^ Int.toString m1
                  
            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
        in
            p_list_sep (string ".") string (m1x :: ms @ [x])
        end

fun p_pat' par env (p, _) =
    case p of
        PVar (s, _) => string s
      | PPrim p => Prim.p_t p
      | PCon (_, pc, _, NONE) => p_patCon env pc
      | PCon (_, pc, cs, SOME p) =>
        if !debug then
            parenIf par (box [p_patCon env pc,
                              string "[",
                              p_list (p_con env) cs,
                              string "]",
                              space,
                              p_pat' true env p])
        else
            parenIf par (box [p_patCon env pc,
                              space,
                              p_pat' true env p])

      | PRecord xps =>
        box [string "{",
             p_list_sep (box [string ",", space]) (fn (x, p, t) =>
                                                      box [string x,
                                                           space,
                                                           string "=",
                                                           space,
                                                           p_pat env p,
                                                           if !debug then
                                                               box [space,
                                                                    string ":",
                                                                    space,
                                                                    p_con env t]
                                                           else
                                                               box []]) xps,
             string "}"]

and p_pat x = p_pat' false x

fun p_exp' par env (e, loc) =
    case e of
        EPrim p => Prim.p_t p
      | ERel n =>
        ((if !debug then
              string (#1 (E.lookupERel env n) ^ "_" ^ Int.toString n)
          else
              string (#1 (E.lookupERel env n)))
         handle E.UnboundRel _ => string ("UNBOUND_REL" ^ Int.toString n))
      | ENamed n =>
        ((if !debug then
              string (#1 (E.lookupENamed env n) ^ "__" ^ Int.toString n)
          else
              string (#1 (E.lookupENamed env n)))
         handle E.UnboundNamed _ => string ("UNBOUND_NAMED" ^ Int.toString n))
      | EModProj (m1, ms, x) =>
        let
            val (m1x, sgn) = E.lookupStrNamed env m1
                handle E.UnboundNamed _ => ("UNBOUND" ^ Int.toString m1, (SgnConst [], loc))

            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
        in
            p_list_sep (string ".") string (m1s :: ms @ [x])
        end
                                         
      | EApp (e1, e2) => parenIf par (box [p_exp env e1,
                                           space,
                                           p_exp' true env e2])
      | EAbs (x, t, _, e) => parenIf par (box [string "fn",
                                               space,
                                               string x,
                                               space,
                                               string ":",
                                               space,
                                               p_con env t,
                                               space,
                                               string "=>",
                                               space,
                                               p_exp (E.pushERel env x t) e])
      | ECApp (e, c) => parenIf par (box [p_exp env e,
                                          space,
                                          string "[",
                                          p_con env c,
                                          string "]"])
      | ECAbs (x, k, e) => parenIf par (box [string "fn",
                                             space,
                                             string x,
                                             space,
                                             string "::",
                                             space,
                                             p_kind env k,
                                             space,
                                             string "=>",
                                             space,
                                             p_exp (E.pushCRel env x k) e])

      | ERecord xes => box [string "{",
                            p_list (fn (x, e, _) =>
                                       box [p_name env x,
                                            space,
                                            string "=",
                                            space,
                                            p_exp env e]) xes,
                            string "}"]
      | EField (e, c, {field, rest}) =>
        if !debug then
            box [p_exp' true env e,
                 string ".",
                 p_con' true env c,
                 space,
                 string "[",
                 p_con env field,
                 space,
                 string " in ",
                 space,
                 p_con env rest,
                 string "]"]
        else
            box [p_exp' true env e,
                 string ".",
                 p_con' true env c]
      | EConcat (e1, c1, e2, c2) =>
        parenIf par (if !debug then
                         box [p_exp' true env e1,
                              space,
                              string ":",
                              space,
                              p_con env c1,
                              space,
                              string "++",
                              space,
                              p_exp' true env e2,
                              space,
                              string ":",
                              space,
                              p_con env c2]
                     else
                         box [p_exp' true env e1,
                              space,
                              string "with",
                              space,
                              p_exp' true env e2])
      | ECut (e, c, {field, rest}) =>
        parenIf par (if !debug then
                         box [p_exp' true env e,
                              space,
                              string "--",
                              space,
                              p_con' true env c,
                              space,
                              string "[",
                              p_con env field,
                              space,
                              string " in ",
                              space,
                              p_con env rest,
                              string "]"]
                     else
                         box [p_exp' true env e,
                              space,
                              string "--",
                              space,
                              p_con' true env c])
      | ECutMulti (e, c, {rest}) =>
        parenIf par (if !debug then
                         box [p_exp' true env e,
                              space,
                              string "---",
                              space,
                              p_con' true env c,
                              space,
                              string "[",
                              p_con env rest,
                              string "]"]
                     else
                         box [p_exp' true env e,
                              space,
                              string "---",
                              space,
                              p_con' true env c])

      | EWrite e => box [string "write(",
                         p_exp env e,
                         string ")"]

      | ECase (e, pes, {disc, result}) =>
        parenIf par (box [string "case",
                          space,
                          p_exp env e,
                          space,
                          if !debug then
                              box [string "in",
                                   space,
                                   p_con env disc,
                                   space,
                                   string "return",
                                   space,
                                   p_con env result,
                                   space]
                          else
                              box [],
                          string "of",
                          space,
                          p_list_sep (box [space, string "|", space])
                                     (fn (p, e) => box [p_pat env p,
                                                        space,
                                                        string "=>",
                                                        space,
                                                        p_exp (E.patBinds env p) e]) pes])

      | ELet (x, t, e1, e2) => box [string "let",
                                    space,
                                    string x,
                                    space,
                                    string ":",
                                    space,
                                    p_con env t,
                                    space,
                                    string "=",
                                    space,
                                    p_exp env e1,
                                    space,
                                    string "in",
                                    newline,
                                    p_exp (E.pushERel env x t) e2]

      | EKAbs (x, e) => box [string x,
                             space,
                             string "==>",
                             space,
                             p_exp (E.pushKRel env x) e]
      | EKApp (e, k) => box [p_exp env e,
                             string "[[",
                             p_kind env k,
                             string "]]"]
                                    

and p_exp env = p_exp' false env

fun p_named x n =
    if !debug then
        box [string x,
             string "__",
             string (Int.toString n)]
    else
        string x

fun p_datatype env (x, n, xs, cons) =
    let
        val k = (KType, ErrorMsg.dummySpan)
        val env = E.pushCNamed env x n k NONE
        val env = foldl (fn (x, env) => E.pushCRel env x k) env xs
    in
        box [string x,
             p_list_sep (box []) (fn x => box [space, string x]) xs,
             space,
             string "=",
             space,
             p_list_sep (box [space, string "|", space])
                        (fn (x, n, NONE) => if !debug then (string (x ^ "__" ^ Int.toString n))
                                            else string x
                          | (x, n, SOME t) => box [if !debug then (string (x ^ "__" ^ Int.toString n))
                                                   else string x, space, string "of", space, p_con env t])
                        cons]
    end

fun p_sgn_item env (sgiAll as (sgi, _)) =
    case sgi of
        SgiConAbs (x, n, k) => box [string "con",
                                    space,
                                    p_named x n,
                                    space,
                                    string "::",
                                    space,
                                    p_kind env k]
      | SgiCon (x, n, k, c) => box [string "con",
                                    space,
                                    p_named x n,
                                    space,
                                    string "::",
                                    space,
                                    p_kind env k,
                                    space,
                                    string "=",
                                    space,
                                    p_con env c]
      | SgiDatatype x => box [string "datatype",
                              space,
                              p_list_sep (box [space, string "and", space]) (p_datatype (E.sgiBinds env sgiAll)) x]
      | SgiDatatypeImp (x, _, m1, ms, x', _, _) =>
        let
            val m1x = #1 (E.lookupStrNamed env m1)
                handle E.UnboundNamed _ => "UNBOUND_STR_" ^ Int.toString m1
        in
            box [string "datatype",
                 space,
                 string x,
                 space,
                 string "=",
                 space,
                 string "datatype",
                 space,
                 p_list_sep (string ".") string (m1x :: ms @ [x'])]
        end
      | SgiVal (x, n, c) => box [string "val",
                                 space,
                                 p_named x n,
                                 space,
                                 string ":",
                                 space,
                                 p_con env c]
      | SgiStr (x, n, sgn) => box [string "structure",
                                   space,
                                   p_named x n,
                                   space,
                                   string ":",
                                   space,
                                   p_sgn env sgn]
      | SgiSgn (x, n, sgn) => box [string "signature",
                                   space,
                                   p_named x n,
                                   space,
                                   string "=",
                                   space,
                                   p_sgn env sgn]

and p_sgn env (sgn, loc) =
    case sgn of
        SgnConst sgis => box [string "sig",
                              newline,
                              let
                                  val (psgis, _) = ListUtil.foldlMap (fn (sgi, env) =>
                                                                         (p_sgn_item env sgi,
                                                                          E.sgiBinds env sgi))
                                                                     env sgis
                              in
                                  p_list_sep newline (fn x => x) psgis
                              end,
                              newline,
                              string "end"]
      | SgnVar n => string ((#1 (E.lookupSgnNamed env n))
                            handle E.UnboundNamed _ => "UNBOUND" ^ Int.toString n)
      | SgnFun (x, n, sgn, sgn') => box [string "functor",
                                         space,
                                         string "(",
                                         p_named x n,
                                         space,
                                         string ":",
                                         space,
                                         p_sgn env sgn,
                                         string ")",
                                         space,
                                         string ":",
                                         space,
                                         p_sgn (E.pushStrNamed env x n sgn) sgn']
      | SgnWhere (sgn, ms, x, c) => box [p_sgn env sgn,
                                     space,
                                     string "where",
                                     space,
                                     string "con",
                                     space,
                                     p_list_sep (string ".") string (ms @ [x]),
                                     space,
                                     string "=",
                                     space,
                                     p_con env c]
      | SgnProj (m1, ms, x) =>
        let
            val (m1x, sgn) = E.lookupStrNamed env m1
                handle E.UnboundNamed _ => ("UNBOUND" ^ Int.toString m1, (SgnConst [], loc))

            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
        in
            p_list_sep (string ".") string (m1x :: ms @ [x])
        end

fun p_vali env (x, n, t, e) = box [p_named x n,
                                   space,
                                   string ":",
                                   space,
                                   p_con env t,
                                   space,
                                   string "=",
                                   space,
                                   p_exp env e]

fun p_decl env (dAll as (d, _) : decl) =
    case d of
        DCon (x, n, k, c) => box [string "con",
                                  space,
                                  p_named x n,
                                  space,
                                  string "::",
                                  space,
                                  p_kind env k,
                                  space,
                                  string "=",
                                  space,
                                  p_con env c]
      | DDatatype x => box [string "datatype",
                            space,
                            p_list_sep (box [space, string "and", space]) (p_datatype (E.declBinds env dAll)) x]
      | DDatatypeImp (x, _, m1, ms, x', _, _) =>
        let
            val m1x = #1 (E.lookupStrNamed env m1)
                handle E.UnboundNamed _ => "UNBOUND_STR_" ^ Int.toString m1
        in
            box [string "datatype",
                 space,
                 string x,
                 space,
                 string "=",
                 space,
                 string "datatype",
                 space,
                 p_list_sep (string ".") string (m1x :: ms @ [x'])]
        end
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
                             
      | DSgn (x, n, sgn) => box [string "signature",
                                 space,
                                 p_named x n,
                                 space,
                                 string "=",
                                 space,
                                 p_sgn env sgn]
      | DStr (x, n, sgn, str) => box [string "structure",
                                      space,
                                      p_named x n,
                                      space,
                                      string ":",
                                      space,
                                      p_sgn env sgn,
                                      space,
                                      string "=",
                                      space,
                                      p_str env str]
      | DFfiStr (x, n, sgn) => box [string "extern",
                                    space,
                                    string "structure",
                                    space,
                                    p_named x n,
                                    space,
                                    string ":",
                                    space,
                                    p_sgn env sgn]
      | DExport (_, sgn, str) => box [string "export",
                                      space,
                                      p_str env str,
                                      space,
                                      string ":",
                                      space,
                                      p_sgn env sgn]
      | DTable (_, x, n, c, pe, _, ce, _) => box [string "table",
                                                  space,
                                                  p_named x n,
                                                  space,
                                                  string ":",
                                                  space,
                                                  p_con env c,
                                                  space,
                                                  string "keys",
                                                  space,
                                                  p_exp env pe,
                                                  space,
                                                  string "constraints",
                                                  space,
                                                  p_exp env ce]
      | DSequence (_, x, n) => box [string "sequence",
                                    space,
                                    p_named x n]
      | DView (_, x, n, e, _) => box [string "view",
                                      space,
                                      p_named x n,
                                      space,
                                      string "as",
                                      space,
                                      p_exp env e]
      | DIndex (e1, e2) => box [string "ensure_index",
                                space,
                                p_exp env e1,
                                space,
                                string ":",
                                space,
                                p_exp env e2]

      | DDatabase s => box [string "database",
                            space,
                            string s]
      | DCookie (_, x, n, c) => box [string "cookie",
                                     space,
                                     p_named x n,
                                     space,
                                     string ":",
                                     space,
                                     p_con env c]
      | DStyle (_, x, n) => box [string "style",
                                 space,
                                 p_named x n]
      | DTask (e1, e2) => box [string "task",
                               space,
                               p_exp env e1,
                               space,
                               string "=",
                               space,
                               p_exp env e2]
      | DPolicy e1 => box [string "policy",
                           space,
                           p_exp env e1]
      | DOnError _ => string "ONERROR"
      | DFfi _ => string "FFI"

and p_str env (str, _) =
    case str of
        StrConst ds => box [string "struct",
                            newline,
                            p_file env ds,
                            newline,
                            string "end"]
      | StrVar n =>
        let
            val x = #1 (E.lookupStrNamed env n)
                    handle E.UnboundNamed _ => "UNBOUND" ^ Int.toString n

            val s = if !debug then
                        x ^ "__" ^ Int.toString n
                    else
                        x
        in
            string s
        end
      | StrProj (str, s) => box [p_str env str,
                                 string ".",
                                 string s]
      | StrFun (x, n, sgn, sgn', str) =>
        let
            val env' = E.pushStrNamed env x n sgn
        in
            box [string "functor",
                 space,
                 string "(",
                 p_named x n,
                 space,
                 string ":",
                 space,
                 p_sgn env sgn,
                 string ")",
                 space,
                 string ":",
                 space,
                 p_sgn env' sgn',
                 space,
                 string "=>",
                 space,
                 p_str env' str]
        end
      | StrApp (str1, str2) => box [p_str env str1,
                                    string "(",
                                    p_str env str2,
                                    string ")"]

and p_file env file =
    let
        val (pds, _) = ListUtil.foldlMap (fn (d, env) =>
                                             (p_decl env d,
                                              E.declBinds env d))
                                         env file
    in
        p_list_sep newline (fn x => x) pds
    end

end
