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

(* Pretty-printing Ur/Web *)

structure SourcePrint :> SOURCE_PRINT = struct

open Print.PD
open Print

open Source

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
      | KUnit => string "Unit"
      | KWild => string "_"
      | KTuple ks => box [string "(",
                          p_list_sep (box [space, string "*", space]) p_kind ks,
                          string ")"]

      | KVar x => string x
      | KFun (x, k) => box [string x,
                            space,
                            string "-->",
                            space,
                            p_kind k]

and p_kind k = p_kind' false k

fun p_explicitness e =
    case e of
        Explicit => string "::"
      | Implicit => string ":::"

fun p_con' par (c, _) =
    case c of
        CAnnot (c, k) => box [string "(",
                              p_con c,
                              space,
                              string "::",
                              space,
                              p_kind k,
                              string ")"]

      | TFun (t1, t2) => parenIf par (box [p_con' true t1,
                                           space,
                                           string "->",
                                           space,
                                           p_con t2])
      | TCFun (e, x, k, c) => parenIf par (box [string x,
                                                space,
                                                p_explicitness e,
                                                space,
                                                p_kind k,
                                                space,
                                                string "->",
                                                space,
                                                p_con c])
      | TRecord (CRecord xcs, _) => box [string "{",
                                         p_list (fn (x, c) =>
                                                    box [p_name x,
                                                         space,
                                                         string ":",
                                                         space,
                                                         p_con c]) xcs,
                                         string "}"]
      | TRecord c => box [string "$",
                          p_con' true c]
      | TDisjoint (c1, c2, c3) => parenIf par (box [string "[",
                                                    p_con c1,
                                                    space,
                                                    string "~",
                                                    space,
                                                    p_con c2,
                                                    string "]",
                                                    space,
                                                    string "=>",
                                                    space,
                                                    p_con c3])

      | CVar (ss, s) => p_list_sep (string ".") string (ss @ [s])
      | CApp (c1, c2) => parenIf par (box [p_con c1,
                                           space,
                                           p_con' true c2])
      | CAbs (x, NONE, c) => parenIf par (box [string "fn",
                                               space,
                                               string x,
                                               space,
                                               string "=>",
                                               space,
                                               p_con c])
      | CAbs (x, SOME k, c) => parenIf par (box [string "fn",
                                                 space,
                                                 string x,
                                                 space,
                                                 string "::",
                                                 space,
                                                 p_kind k,
                                                 space,
                                                 string "=>",
                                                 space,
                                                 p_con c])


      | CName s => box [string "#", string s]

      | CRecord xcs => box [string "[",
                            p_list (fn (x, c) =>
                                       box [p_con x,
                                            space,
                                            string "=",
                                            space,
                                            p_con c]) xcs,
                            string "]"]
      | CConcat (c1, c2) => parenIf par (box [p_con' true c1,
                                              space,
                                              string "++",
                                              space,
                                              p_con c2])
      | CMap => string "map"

      | CUnit => string "()"

      | CWild k => box [string "(_",
                        space,
                        string "::",
                        space,
                        p_kind k,
                        string ")"]

      | CTuple cs => box [string "(",
                          p_list p_con cs,
                          string ")"]
      | CProj (c, n) => box [p_con c,
                             string ".",
                             string (Int.toString n)]

      | CKAbs (x, c) => box [string x,
                             space,
                             string "==>",
                             space,
                             p_con c]
      | TKFun (x, c) => box [string x,
                             space,
                             string "-->",
                             space,
                             p_con c]
        
and p_con c = p_con' false c

and p_name (all as (c, _)) =
    case c of
        CName s => string s
      | _ => p_con all

fun p_pat' par (p, _) =
    case p of
        PWild => string "_"
      | PVar s => string s
      | PPrim p => Prim.p_t p
      | PCon (ms, x, NONE) => p_list_sep (string ".") string (ms @ [x])
      | PCon (ms, x, SOME p) => parenIf par (box [p_list_sep (string ".") string (ms @ [x]),
                                                  space,
                                                  p_pat' true p])
      | PRecord (xps, flex) =>
        let
            val pps = map (fn (x, p) => box [string x, space, string "=", space, p_pat p]) xps
        in
            box [string "{",
                 p_list_sep (box [string ",", space]) (fn x => x)
                 (if flex then
                      pps @ [string "..."]
                  else
                      pps),
                 string "}"]
        end

      | PAnnot (p, t) => box [p_pat p,
                              space,
                              string ":",
                              space,
                              p_con t]

and p_pat x = p_pat' false x

fun p_exp' par (e, _) =
    case e of
        EAnnot (e, t) => box [string "(",
                              p_exp e,
                              space,
                              string ":",
                              space,
                              p_con t,
                              string ")"]        

      | EPrim p => Prim.p_t p
      | EVar (ss, s, _) => p_list_sep (string ".") string (ss @ [s])
      | EApp (e1, e2) => parenIf par (box [p_exp e1,
                                           space,
                                           p_exp' true e2])
      | EAbs (x, NONE, e) => parenIf par (box [string "fn",
                                               space,
                                               string x,
                                               space,
                                               string "=>",
                                               space,
                                               p_exp e])
      | EAbs (x, SOME t, e) => parenIf par (box [string "fn",
                                                 space,
                                                 string x,
                                                 space,
                                                 string ":",
                                                 space,
                                                 p_con t,
                                                 space,
                                                 string "=>",
                                                 space,
                                                 p_exp e])
      | ECApp (e, c) => parenIf par (box [p_exp e,
                                          space,
                                          string "[",
                                          p_con c,
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
                                                  p_exp e])
      | EDisjoint (c1, c2, e) => parenIf par (box [p_con c1,
                                                   space,
                                                   string "~",
                                                   space,
                                                   p_con c2,
                                                   space,
                                                   string "=>",
                                                   space,
                                                   p_exp e])
      | EDisjointApp e => parenIf par (box [p_exp e,
                                            space,
                                            string "!"])

      | ERecord xes => box [string "{",
                            p_list (fn (x, e) =>
                                       box [p_name x,
                                            space,
                                            string "=",
                                            space,
                                            p_exp e]) xes,
                            string "}"]
      | EField (e, c) => box [p_exp' true e,
                              string ".",
                              p_con' true c]
      | EConcat (e1, e2) => parenIf par (box [p_exp' true e1,
                                              space,
                                              string "++",
                                              space,
                                              p_exp' true e2])
      | ECut (e, c) => parenIf par (box [p_exp' true e,
                                         space,
                                         string "--",
                                         space,
                                         p_con' true c])
      | ECutMulti (e, c) => parenIf par (box [p_exp' true e,
                                              space,
                                              string "---",
                                              space,
                                              p_con' true c])
      | ECase (e, pes) => parenIf par (box [string "case",
                                            space,
                                            p_exp e,
                                            space,
                                            string "of",
                                            space,
                                            p_list_sep (box [space, string "|", space])
                                            (fn (p, e) => box [p_pat p,
                                                               space,
                                                               string "=>",
                                                               space,
                                                               p_exp e]) pes])

      | EWild => string "_"

      | ELet (ds, e) => box [string "let",
                             newline,
                             box [p_list_sep newline p_edecl ds],
                             newline,
                             string "in",
                             newline,
                             box [p_exp e],
                             newline,
                             string "end"]

      | EKAbs (x, e) => box [string x,
                             space,
                             string "-->",
                             space,
                             p_exp e]

and p_exp e = p_exp' false e

and p_edecl (d, _) =
  case d of
      EDVal (p, e) => box [string "val",
                           space,
                           p_pat p,
                           space,
                           string "=",
                           space,
                           p_exp e]
    | EDValRec vis => box [string "val",
                           space,
                           string "rec",
                           space,
                           p_list_sep (box [newline, string "and", space]) p_vali vis]

and p_vali (x, co, e) =
    case co of
        NONE => box [string x,
                     space,
                     string "=",
                     space,
                     p_exp e]
      | SOME t => box [string x,
                       space,
                       string ":",
                       space,
                       p_con t,
                       space,
                       string "=",
                       space,
                       p_exp e]


fun p_datatype (x, xs, cons) =
    box [string x,
         p_list_sep (box []) (fn x => box [space, string x]) xs,
         space,
         string "=",
         space,
         p_list_sep (box [space, string "|", space])
         (fn (x, NONE) => string x
           | (x, SOME t) => box [string x, space, string "of", space, p_con t])
         cons]

fun p_sgn_item (sgi, _) =
    case sgi of
        SgiConAbs (x, k) => box [string "con",
                                 space,
                                 string x,
                                 space,
                                 string "::",
                                 space,
                                 p_kind k]
      | SgiCon (x, NONE, c) => box [string "con",
                                    space,
                                    string x,
                                    space,
                                    string "=",
                                    space,
                                    p_con c]
      | SgiCon (x, SOME k, c) => box [string "con",
                                      space,
                                      string x,
                                      space,
                                      string "::",
                                      space,
                                      p_kind k,
                                      space,
                                      string "=",
                                      space,
                                      p_con c]
      | SgiDatatype x => box [string "datatype",
                              space,
                              p_list_sep (box [space, string "and", space]) p_datatype x]
      | SgiDatatypeImp (x, ms, x') =>
        box [string "datatype",
             space,
             string x,
             space,
             string "=",
             space,
             string "datatype",
             space,
             p_list_sep (string ".") string (ms @ [x'])]
      | SgiVal (x, c) => box [string "val",
                              space,
                              string x,
                              space,
                              string ":",
                              space,
                              p_con c]
      | SgiTable (x, c, pe, ce) => box [string "table",
                                        space,
                                        string x,
                                        space,
                                        string ":",
                                        space,
                                        p_con c,
                                        space,
                                        string "keys",
                                        space,
                                        p_exp pe,
                                        space,
                                        string "constraints",
                                        space,
                                        p_exp ce]
      | SgiStr (x, sgn) => box [string "structure",
                                space,
                                string x,
                                space,
                                string ":",
                                space,
                                p_sgn sgn]
      | SgiSgn (x, sgn) => box [string "signature",
                                space,
                                string x,
                                space,
                                string "=",
                                space,
                                p_sgn sgn]
      | SgiInclude sgn => box [string "include",
                               space,
                               p_sgn sgn]
      | SgiConstraint (c1, c2) => box [string "constraint",
                                       space,
                                       p_con c1,
                                       space,
                                       string "~",
                                       space,
                                       p_con c2]
      | SgiClassAbs (x, k) => box [string "class",
                                   space,
                                   string x,
                                   space,
                                   string "::",
                                   space,
                                   p_kind k]
      | SgiClass (x, k, c) => box [string "class",
                                   space,
                                   string x,
                                   space,
                                   string "::",
                                   space,
                                   p_kind k,
                                   space,
                                   string "=",
                                   space,
                                   p_con c]
                              
and p_sgn (sgn, _) =
    case sgn of
        SgnConst sgis => box [string "sig",
                              newline,
                              p_list_sep newline p_sgn_item sgis,
                              newline,
                              string "end"]
      | SgnVar x => string x
      | SgnFun (x, sgn, sgn') => box [string "functor",
                                      space,
                                      string "(",
                                      string x,
                                      space,
                                      string ":",
                                      p_sgn sgn,
                                      string ")",
                                      space,
                                      string ":",
                                      space,
                                      p_sgn sgn']
      | SgnWhere (sgn, x, c) => box [p_sgn sgn,
                                     space,
                                     string "where",
                                     space,
                                     string "con",
                                     space,
                                     string x,
                                     space,
                                     string "=",
                                     space,
                                     p_con c]
      | SgnProj (m, ms, x) => p_list_sep (string ".") string (m :: ms @ [x])
                                   


fun p_decl ((d, _) : decl) =
    case d of
        DCon (x, NONE, c) => box [string "con",
                                  space,
                                  string x,
                                  space,
                                  string "=",
                                  space,
                                  p_con c]
      | DCon (x, SOME k, c) => box [string "con",
                                    space,
                                    string x,
                                    space,
                                    string "::",
                                    space,
                                    p_kind k,
                                    space,
                                    string "=",
                                    space,
                                    p_con c]
      | DDatatype x => box [string "datatype",
                            space,
                            p_list_sep (box [space, string "and", space]) p_datatype x]
      | DDatatypeImp (x, ms, x') =>
        box [string "datatype",
             space,
             string x,
             space,
             string "=",
             space,
             string "datatype",
             space,
             p_list_sep (string ".") string (ms @ [x'])]
      | DVal vi => box [string "val",
                        space,
                        p_vali vi]
      | DValRec vis => box [string "val",
                            space,
                            string "rec",
                            space,
                            p_list_sep (box [newline, string "and", space]) p_vali vis]

      | DSgn (x, sgn) => box [string "signature",
                              space,
                              string x,
                              space,
                              string "=",
                              space,
                              p_sgn sgn]
      | DStr (x, NONE, _, str) => box [string "structure",
                                       space,
                                       string x,
                                       space,
                                       string "=",
                                       space,
                                       p_str str]
      | DStr (x, SOME sgn, _, str) => box [string "structure",
                                           space,
                                           string x,
                                           space,
                                           string ":",
                                           space,
                                           p_sgn sgn,
                                           space,
                                           string "=",
                                           space,
                                           p_str str]
      | DFfiStr (x, sgn, _) => box [string "extern",
                                    space,
                                    string "structure",
                                    space,
                                    string x,
                                    space,
                                    string ":",
                                    space,
                                    p_sgn sgn]
      | DOpen (m, ms) => box [string "open",
                              space,
                              p_list_sep (string ".") string (m :: ms)]
      | DConstraint (c1, c2) => box [string "constraint",
                                     space,
                                     p_con c1,
                                     space,
                                     string "~",
                                     space,
                                     p_con c2]
      | DOpenConstraints (m, ms) => box [string "open",
                                         space,
                                         string "constraints",
                                         space,
                                         p_list_sep (string ".") string (m :: ms)]

      | DExport str => box [string "export",
                            space,
                            p_str str]
      | DTable (x, c, pe, ce) => box [string "table",
                                      space,
                                      string x,
                                      space,
                                      string ":",
                                      space,
                                      p_con c,
                                      space,
                                      string "keys",
                                      space,
                                      p_exp pe,
                                      space,
                                      string "constraints",
                                      space,
                                      p_exp ce]
      | DSequence x => box [string "sequence",
                            space,
                            string x]
      | DView (x, e) => box [string "view",
                             space,
                             string x,
                             space,
                             string "=",
                             space,
                             p_exp e]
      | DClass (x, k, c) => box [string "class",
                                 space,
                                 string x,
                                 space,
                                 string "=",
                                 space,
                                 p_con c]

      | DDatabase s => box [string "database",
                            space,
                            string s]

      | DCookie (x, c) => box [string "cookie",
                               space,
                               string x,
                               space,
                               string ":",
                               space,
                               p_con c]
      | DStyle x => box [string "style",
                         space,
                         string x]
      | DTask (e1, e2) => box [string "task",
                               space,
                               p_exp e1,
                               space,
                               string "=",
                               space,
                               p_exp e2]
      | DPolicy e1 => box [string "policy",
                           space,
                           p_exp e1]
      | DOnError _ => string "ONERROR"

and p_str (str, _) =
    case str of
        StrConst ds => box [string "struct",
                            newline,
                            p_list_sep newline p_decl ds,
                            newline,
                            string "end"]
      | StrVar x => string x
      | StrProj (str, x) => box [p_str str,
                                 string ".",
                                 string x]
      | StrFun (x, sgn, NONE, str) => box [string "functor",
                                           space,
                                           string "(",
                                           string x,
                                           space,
                                           string ":",
                                           p_sgn sgn,
                                           string ")",
                                           space,
                                           string "=>",
                                           space,
                                           p_str str]
      | StrFun (x, sgn, SOME sgn', str) => box [string "functor",
                                                space,
                                                string "(",
                                                string x,
                                                space,
                                                string ":",
                                                p_sgn sgn,
                                                string ")",
                                                space,
                                                string ":",
                                                space,
                                                p_sgn sgn',
                                                space,
                                                string "=>",
                                                space,
                                                p_str str]
      | StrApp (str1, str2) => box [p_str str1,
                                    string "(",
                                    p_str str2,
                                    string ")"]

val p_file = p_list_sep newline p_decl

end
