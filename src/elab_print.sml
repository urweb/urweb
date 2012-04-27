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

(* Pretty-printing elaborated Ur/Web *)

structure ElabPrint :> ELAB_PRINT = struct

open Print.PD
open Print

open Elab

structure E = ElabEnv

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

      | KError => string "<ERROR>"
      | KUnif (_, _, ref (KKnown k)) => p_kind' par env k
      | KUnif (_, s, _) => string ("<UNIF:" ^ s ^ ">")
      | KTupleUnif (_, _, ref (KKnown k)) => p_kind' par env k
      | KTupleUnif (_, nks, _) => box [string "(",
                                       p_list_sep (box [space, string "*", space])
                                                  (fn (n, k) => box [string (Int.toString n ^ ":"),
                                                                     space,
                                                                     p_kind env k]) nks,
                                       space,
                                       string "*",
                                       space,
                                       string "...)"]

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
                                                p_kind env k,
                                                space,
                                                string "->",
                                                space,
                                                p_con (E.pushCRel env x k) c])
      | TDisjoint (c1, c2, c3) => parenIf par (box [string "[",
                                                    p_con env c1,
                                                    space,
                                                    string "~",
                                                    space,
                                                    p_con env c2,
                                                    string "]",
                                                    space,
                                                    string "=>",
                                                    space,
                                                    p_con env c3])
      | TRecord (CRecord (_, xcs), _) =>
        let
            fun isTuple (n, xcs) =
                case xcs of
                    [] => n > 2
                  | ((CName s, _), _) :: xcs' =>
                    s = Int.toString n andalso isTuple (n+1, xcs')
                  | _ => false
        in
            if isTuple (1, xcs) then
                case xcs of
                    (_, c) :: xcs =>
                    parenIf par (box [p_con' true env c,
                                      p_list_sep (box []) (fn (_, c) => box [space,
                                                                             string "*",
                                                                             space,
                                                                             p_con' true env c]) xcs])
                  | _ => raise Fail "ElabPrint: surprise empty tuple"
            else
                box [string "{",
                     p_list (fn (x, c) =>
                                box [p_name env x,
                                     space,
                                     string ":",
                                     space,
                                     p_con env c]) xcs,
                     string "}"]
        end
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
                handle E.UnboundNamed _ => "UNBOUND_STR_" ^ Int.toString m1
               
            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
        in
            if m1x = "Basis" andalso (case E.lookupC env x of
                                          E.Named (n, _) =>
                                          let
                                              val (_, _, co) = E.lookupCNamed env n
                                          in
                                              case co of
                                                  SOME (CModProj (m1', [], x'), _) => m1' = m1 andalso x' = x
                                                | _ => false
                                          end
                                        | E.NotBound => true
                                        | _ => false) then
                string x
            else
                p_list_sep (string ".") string (m1s :: ms @ [x])
        end 

      | CApp (c1, c2) => parenIf par (box [p_con env c1,
                                           space,
                                           p_con' true env c2])
      | CAbs (x, k, c) => parenIf true (box [string "fn",
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
                                         box [p_name env x,
                                              space,
                                              string "=",
                                              space,
                                              p_con env c]) xcs,
                              string "]::",
                              p_kind env k])
        else
            parenIf par (box [string "[",
                              p_list (fn (x, c) =>
                                         box [p_name env x,
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

      | CError => string "<ERROR>"
      | CUnif (nl, _, _, _, ref (Known c)) => p_con' par env (E.mliftConInCon nl c)
      | CUnif (nl, _, k, s, _) => box [string ("<UNIF:" ^ s ^ "::"),
                                       p_kind env k,
                                       case nl of
                                           0 => box []
                                         | _ => string ("+" ^ Int.toString nl),
                                       string ">"]

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
        PWild => string "_"
      | PVar (s, _) => string s
      | PPrim p => Prim.p_t p
      | PCon (_, pc, _, NONE) => p_patCon env pc
      | PCon (_, pc, _, SOME p) => parenIf par (box [p_patCon env pc,
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

fun p_exp' par env (e, _) =
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
            val m1x = #1 (E.lookupStrNamed env m1)
                handle E.UnboundNamed _ => "UNBOUND_STR_" ^ Int.toString m1
                  
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
                              string "++",
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

      | ECase (e, pes, _) => parenIf par (box [string "case",
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

      | EError => string "<ERROR>"
      | EUnif (ref (SOME e)) => p_exp env e
      | EUnif _ => string "_"

      | ELet (ds, e, _) =>
        let
            val (dsp, env) = ListUtil.foldlMap
                             (fn (d, env) =>
                                 (p_edecl env d,
                                  E.edeclBinds env d))
                             env ds
        in
            box [string "let",
                 newline,
                 box [p_list_sep newline (fn x => x) dsp],
                 newline,
                 string "in",
                 newline,
                 box [p_exp env e],
                 newline,
                 string "end"]
        end

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

and p_edecl env (dAll as (d, _)) =
    case d of
        EDVal (p, t, e) => box [string "val",
                                space,
                                p_pat env p,
                                space,
                                string ":",
                                space,
                                p_con env t,
                                space,
                                string "=",
                                space,
                                p_exp env e]
      | EDValRec vis =>
        let
            val env = E.edeclBinds env dAll
        in
            box [string "val",
                 space,
                 string "rec",
                 space,
                 p_list_sep (box [newline, string "and", space]) (p_evali env) vis]
        end

and p_evali env (x, t, e) = box [string x,
                                 space,
                                 string ":",
                                 space,
                                 p_con env t,
                                 space,
                                 string "=",
                                 space,
                                 p_exp env e]

fun p_datatype env (x, n, xs, cons) =
    let
        val k = (KType, ErrorMsg.dummySpan)
        val env = E.pushCNamedAs env x n k NONE
        val env = foldl (fn (x, env) => E.pushCRel env x k) env xs
    in
        box [string x,
             p_list_sep (box []) (fn x => box [space, string x]) xs,
             space,
             string "=",
             space,
             p_list_sep (box [space, string "|", space])
                        (fn (x, _, NONE) => string x
                          | (x, _, SOME t) => box [string x, space, string "of", space, p_con env t])
                        cons]
    end

fun p_named x n =
    if !debug then
        box [string x,
             string "__",
             string (Int.toString n)]
    else
        string x

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
      | SgiConstraint (c1, c2) => box [string "constraint",
                                       space,
                                       p_con env c1,
                                       space,
                                       string "~",
                                       space,
                                       p_con env c2]
      | SgiClassAbs (x, n, k) => box [string "class",
                                      space,
                                      p_named x n,
                                      space,
                                      string "::",
                                      space,
                                      p_kind env k]
      | SgiClass (x, n, k, c) => box [string "class",
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
      | SgnVar n => ((string (#1 (E.lookupSgnNamed env n)))
                     handle E.UnboundNamed _ => string ("UNBOUND_SGN_" ^ Int.toString n))
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
            val m1x = #1 (E.lookupStrNamed env m1)
                handle E.UnboundNamed _ => "UNBOUND_SGN_" ^ Int.toString m1
                   
            val m1s = if !debug then
                          m1x ^ "__" ^ Int.toString m1
                      else
                          m1x
         in
            p_list_sep (string ".") string (m1x :: ms @ [x])
        end
      | SgnError => string "<ERROR>"

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
      | DConstraint (c1, c2) => box [string "constraint",
                                     space,
                                     p_con env c1,
                                     space,
                                     string "~",
                                     space,
                                     p_con env c2]
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
      | DClass (x, n, k, c) => box [string "class",
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

and p_str env (str, _) =
    case str of
        StrConst ds => box [string "struct",
                            newline,
                            p_file env ds,
                            newline,
                            string "end"]
      | StrVar n => ((string (#1 (E.lookupStrNamed env n)))
                     handle E.UnboundNamed _ => string ("UNBOUND_STR_" ^ Int.toString n))
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
