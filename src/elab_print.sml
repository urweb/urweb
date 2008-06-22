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

(* Pretty-printing elaborated Laconic/Web *)

structure ElabPrint :> ELAB_PRINT = struct

open Print.PD
open Print

open Elab

structure E = ElabEnv

val debug = ref false

fun p_kind' par (k, _) =
    case k of
        KType => string "Type"
      | KArrow (k1, k2) => parenIf par (box [p_kind' true k1,
                                             space,
                                             string "->",
                                             space,
                                             p_kind k2])
      | KName => string "Name"
      | KRecord k => box [string "{", p_kind k, string "}"]

      | KError => string "<ERROR>"
      | KUnif (_, ref (SOME k)) => p_kind' par k
      | KUnif (s, _) => string ("<UNIF:" ^ s ^ ">")

and p_kind k = p_kind' false k

fun p_explicitness e =
    case e of
        Explicit => string "::"
      | Implicit => string ":::"

fun p_con' par env (c, _) =
    case c of
        TFun (t1, t2) => parenIf par (box [p_con' true env t1,
                                           space,
                                           string "->",
                                           space,
                                           p_con env t2])
      | TCFun (e, x, k, c) => parenIf par (box [string x,
                                                space,
                                                p_explicitness e,
                                                space,
                                                p_kind k,
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
        if !debug then
            string (#1 (E.lookupCRel env n) ^ "_" ^ Int.toString n)
        else
            string (#1 (E.lookupCRel env n))
      | CNamed n =>
        ((if !debug then
              string (#1 (E.lookupCNamed env n) ^ "__" ^ Int.toString n)
          else
              string (#1 (E.lookupCNamed env n)))
         handle E.UnboundNamed _ => string ("UNBOUND_NAMED" ^ Int.toString n))
      | CModProj (m1, ms, x) =>
        let
            val (m1x, sgn) = E.lookupStrNamed env m1

            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
        in
            p_list_sep (string ".") string (m1x :: ms @ [x])
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
                                            p_kind k,
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
                              p_kind k])
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

      | CError => string "<ERROR>"
      | CUnif (_, _, ref (SOME c)) => p_con' par env c
      | CUnif (k, s, _) => box [string ("<UNIF:" ^ s ^ "::"),
                               p_kind k,
                               string ">"]
        
and p_con env = p_con' false env

and p_name env (all as (c, _)) =
    case c of
        CName s => string s
      | _ => p_con env all

fun p_exp' par env (e, _) =
    case e of
        EPrim p => Prim.p_t p
      | ERel n =>
        if !debug then
            string (#1 (E.lookupERel env n) ^ "_" ^ Int.toString n)
        else
            string (#1 (E.lookupERel env n))
      | ENamed n =>
        if !debug then
            string (#1 (E.lookupENamed env n) ^ "__" ^ Int.toString n)
        else
            string (#1 (E.lookupENamed env n))
      | EModProj (m1, ms, x) =>
        let
            val (m1x, sgn) = E.lookupStrNamed env m1

            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
        in
            p_list_sep (string ".") string (m1x :: ms @ [x])
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
      | ECAbs (exp, x, k, e) => parenIf par (box [string "fn",
                                                  space,
                                                  string x,
                                                  space,
                                                  p_explicitness exp,
                                                  space,
                                                  p_kind k,
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
            
      | EError => string "<ERROR>"

and p_exp env = p_exp' false env

fun p_named x n =
    if !debug then
        box [string x,
             string "__",
             string (Int.toString n)]
    else
        string x

fun p_sgn_item env (sgi, _) =
    case sgi of
        SgiConAbs (x, n, k) => box [string "con",
                                    space,
                                    p_named x n,
                                    space,
                                    string "::",
                                    space,
                                    p_kind k]
      | SgiCon (x, n, k, c) => box [string "con",
                                    space,
                                    p_named x n,
                                    space,
                                    string "::",
                                    space,
                                    p_kind k,
                                    space,
                                    string "=",
                                    space,
                                    p_con env c]
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

and p_sgn env (sgn, _) =
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
      | SgnVar n => string (#1 (E.lookupSgnNamed env n))
      | SgnFun (x, n, sgn, sgn') => box [string "functor",
                                         space,
                                         string "(",
                                         string x,
                                         space,
                                         string ":",
                                         space,
                                         p_sgn env sgn,
                                         string ")",
                                         space,
                                         string ":",
                                         space,
                                         p_sgn (E.pushStrNamedAs env x n sgn) sgn']
      | SgnWhere (sgn, x, c) => box [p_sgn env sgn,
                                     space,
                                     string "where",
                                     space,
                                     string "con",
                                     space,
                                     string x,
                                     space,
                                     string "=",
                                     space,
                                     p_con env c]
      | SgnProj (m1, ms, x) =>
        let
            val (m1x, sgn) = E.lookupStrNamed env m1

            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
        in
            p_list_sep (string ".") string (m1x :: ms @ [x])
        end
      | SgnError => string "<ERROR>"

fun p_decl env ((d, _) : decl) =
    case d of
        DCon (x, n, k, c) => box [string "con",
                                  space,
                                  p_named x n,
                                  space,
                                  string "::",
                                  space,
                                  p_kind k,
                                  space,
                                  string "=",
                                  space,
                                  p_con env c]
      | DVal (x, n, t, e) => box [string "val",
                                  space,
                                  p_named x n,
                                  space,
                                  string ":",
                                  space,
                                  p_con env t,
                                  space,
                                  string "=",
                                  space,
                                  p_exp env e]
                             
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

and p_str env (str, _) =
    case str of
        StrConst ds => box [string "struct",
                            newline,
                            p_file env ds,
                            newline,
                            string "end"]
      | StrVar n => string (#1 (E.lookupStrNamed env n))
      | StrProj (str, s) => box [p_str env str,
                                 string ".",
                                 string s]
      | StrFun (x, n, sgn, sgn', str) =>
        let
            val env' = E.pushStrNamedAs env x n sgn
        in
            box [string "functor",
                 space,
                 string "(",
                 string x,
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
      | StrError => string "<ERROR>"

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
