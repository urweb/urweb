(* Copyright (c) 2012, Adam Chlipala
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

structure GetInfo :> GET_INFO = struct 

structure U = ElabUtilPos
structure E = ElabEnv
structure L = Elab
structure P = Print

fun isPosIn file row col span =
    let
        val start = #first span
        val end_ = #last span
    in
        String.isSuffix file (#file span)
        andalso
        (#line start < row orelse
         #line start = row andalso #char start <= col)
        andalso 
        (#line end_ > row orelse
         #line end_ = row andalso #char end_ >= col)

    end

fun isSmallerThan (s1: ErrorMsg.span) (s2: ErrorMsg.span) =
    (#line (#first s1) > #line (#first s2) orelse
     (#line (#first s1) = #line (#first s2) andalso (#char (#first s1) >= #char (#first s2))))
    andalso 
    (#line (#last s1) < #line (#last s2) orelse
     (#line (#last s1) = #line (#last s2) andalso (#char (#last s1) <= #char (#last s2)))) 

datatype item =
         Kind of L.kind
         | Con of L.con
         | Exp of L.exp
         | Sgn_item of L.sgn_item
         | Sgn of L.sgn
         | Str of L.str
         | Decl of L.decl

fun getSpan (f: item * E.env) =
    case #1 f of
        Kind k => #2 k
      | Con c => #2 c
      | Exp e => #2 e
      | Sgn_item si => #2 si
      | Sgn s => #2 s
      | Str s => #2 s
      | Decl d => #2 d

fun getInfo' file row col =
    if not (!Elaborate.incremental)
    then P.PD.string "ERROR: urweb daemon is needed to use typeOf command"
    else
        case ModDb.lookupModAndDepsIncludingErrored (Compiler.moduleOf file) of
            NONE => P.PD.string ("ERROR: No module found: " ^ Compiler.moduleOf file)
          | SOME (modDecl, deps) => 
            let
                val () = U.mliftConInCon := E.mliftConInCon

                (* Adding signature of dependencies to environment *)
                val env = List.foldl (fn (d, e) => E.declBinds e d) E.empty deps

                (* Adding previous declarations in file to environment *)
                (* "open <mod>" statements are already translated during elaboration *)
                (*   They get added to the env here "unprefixed" *)
                val env =
                    case #1 modDecl of 
                        L.DStr (name, _, sgn, str) =>
                        (case #1 str of
                             L.StrConst decls =>
                             List.foldl (fn (d, env) =>
                                            if #line (#first (#2 d)) <= row
                                               andalso #char (#first (#2 d)) <= col
                                            then E.declBinds env d
                                            else env) env decls
                           | _ => env)
                      | L.DFfiStr _ => env
                      | _ => env

                (* Basis and Top need to be added to the env explicitly *)
                val env =
                    case ModDb.lookupModAndDepsIncludingErrored  "Top" of
                        NONE => raise Fail "ERROR: Top module not found in ModDb"
                      | SOME ((L.DStr (_, top_n, topSgn, topStr), _), _) => 
                        #2 (Elaborate.dopen env {str = top_n, strs = [], sgn = topSgn})
                      | _ => raise Fail "ERROR: Impossible"
                val env =
                    case ModDb.lookupModAndDepsIncludingErrored "Basis" of
                        NONE => raise Fail "ERROR: Top module not found in ModDb"
                      | SOME ((L.DFfiStr (_, basis_n, sgn), _), _) =>
                        #2 (Elaborate.dopen env {str = basis_n, strs = [], sgn = sgn})
                      | _ => raise Fail "ERROR: Impossible"

                (* Just use ElabPrint functions. *)
                (* These are better for compiler error message, but it's better than nothing *)
                fun printLiterally {span = span, item = item, env = env} =
                    P.box [ case item of
                                Kind k => P.box [P.PD.string "KIND: ", ElabPrint.p_kind env k]
                              | Con c => P.box [P.PD.string "CON: ", ElabPrint.p_con env c]
                              | Exp e => P.box [P.PD.string "EXP: ", ElabPrint.p_exp env e]
                              | Sgn_item si => P.box [P.PD.string "SGN_ITEM: ", ElabPrint.p_sgn_item env si]
                              | Sgn s => P.box [P.PD.string "SGN: ", ElabPrint.p_sgn env s]
                              | Str s => P.box [P.PD.string "STR: ", ElabPrint.p_str env s]
                              | Decl d => P.box [P.PD.string "DECL: ", ElabPrint.p_decl env d]
                          ]

                (* TODO We lose some really useful information, like eg. inferred parameters, *)
                (*   which we do have in the actual items (L.Decl, L.Exp, etc) *)
                (*   but not when we do a lookup into the Env *)
                (* TODO Rename? *)
                fun printGoodPart env f span =
                    (case f of
                         Exp (L.EPrim p, _) =>
                         SOME (P.box [Prim.p_t p,
                                      P.PD.string ": ",
                                      P.PD.string (case p of
                                                       Prim.Int _ => "int"
                                                     | Prim.Float _ => "float"
                                                     | Prim.String _ => "string"
                                                     | Prim.Char _ => "char")])
                       | Exp (L.ERel n, _) =>
                         SOME ((let val found = E.lookupERel env n
                                in
                                    P.box [ P.PD.string (#1 found)
                                          , P.PD.string ": "
                                          , ElabPrint.p_con env (#2 found)]
                                end)
                               handle E.UnboundRel _ => P.PD.string ("UNBOUND_REL" ^ Int.toString n))
                       | Exp (L.ENamed n, _) =>
                         SOME ((let val found = E.lookupENamed env n
                                in
                                    P.box [ P.PD.string (#1 found)
                                          , P.PD.string ": "
                                          , ElabPrint.p_con env (#2 found)]
                                end)
                               handle E.UnboundNamed _ => P.PD.string ("UNBOUND_NAMED" ^ Int.toString n))
                       | Exp (L.EModProj ( m1 (* number (= "name") of top level module *)
                                        , ms (* names of submodules - possibly none *)
                                        , x (* identifier *)), loc) =>
                         SOME (let
                                  val (m1name, m1sgn) = E.lookupStrNamed env m1
                                  val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                                             case E.projectStr env {sgn = sgn, str = str, field = m} of
                                                                 NONE => raise Fail ("Couldn't find Structure: " ^ m)
                                                               | SOME sgn => ((L.StrProj (str, m), loc), sgn))
                                                         ((L.StrVar m1, loc), m1sgn)
                                                         ms
                                  val t = case E.projectVal env {sgn = sgn, str = str, field = x} of
                                              NONE => raise Fail ("Couldn't find identifier: " ^ x)
                                            | SOME t => t
                              in
                                  P.box [ P.p_list_sep (P.PD.string ".") P.PD.string (m1name :: ms @ [x])
                                        , P.PD.string ": "
                                        , ElabPrint.p_con env t
                                        ]
                              end
                               handle E.UnboundNamed _ => P.PD.string ("Module not found: " ^ Int.toString m1))
                       | Exp e => NONE
                       | Kind k => NONE
                       | Con c => NONE
                       | Sgn_item si => NONE
                       | Sgn s => NONE
                       | Str s => NONE
                       | Decl d => NONE)

                fun add env item span acc =
                           if not (isPosIn file row col span)
                           then
                               acc
                           else
                               let
                                   val smallest =
                                       if isSmallerThan span (#span (#smallest acc))
                                       then {span = span, item = item, env = env}
                                       else #smallest acc
                                   val smallestgoodpart =
                                       case #smallestgoodpart acc of
                                           NONE => 
                                           (case printGoodPart env item span of
                                                NONE => NONE
                                              | SOME desc => SOME (desc, span))
                                         | SOME (desc', span') => 
                                           if isSmallerThan span span'
                                           then 
                                               (case printGoodPart env item span of
                                                    NONE => SOME (desc', span')
                                                  | SOME desc => SOME (desc, span))
                                           else SOME (desc', span')
                               in
                                   {smallest = smallest, smallestgoodpart = smallestgoodpart}
                               end

                (* Look for item at input position *)
                (* We're looking for two things simultaneously: *)
                (* 1. The "smallest" part, ie. the one of which the source span is the smallest *)
                (* 2. The "smallestgoodpart" part, ie. the one of which the source span is the smallest AND has a special case in printGoodPart *)
                (* If we end up with a smallestgoodpart, we'll show that one since that one is probably more useful  *)
                (* TODO source spans of XML and SQL sources are weird and you end *)
                (*   up with eg: a span from eg 1-5 and another from 2-6, makes no sense? *)
                (*   That's one of the reasons why we're searching for the two things mentioned above *)
                val result =
                    U.Decl.foldB
                        { kind = fn (env, (k, span), acc) => add env (Kind (k, span)) span acc,
                          con = fn (env, (k, span), acc) => add env (Con (k, span)) span acc,
                          exp = fn (env, (k, span), acc) => add env (Exp (k, span)) span acc,
                          sgn_item = fn (env, (k, span), acc) => add env (Sgn_item (k, span)) span acc,
                          sgn = fn (env, (k, span), acc) => add env (Sgn (k, span)) span acc,
                          str = fn (env, (k, span), acc) => add env (Str (k, span)) span acc,
                          decl = fn (env, (k, span), acc) => add env (Decl (k, span)) span acc,
                          bind = fn (env, binder) =>
                                    case binder of
                                        U.Decl.RelK x => E.pushKRel env x
                                      | U.Decl.RelC (x, k) => E.pushCRel env x k
                                      | U.Decl.NamedC (x, n, k, co) => E.pushCNamedAs env x n k co
                                      | U.Decl.RelE (x, c) => E.pushERel env x c
                                      | U.Decl.NamedE (x, c) => #1 (E.pushENamed env x c)
                                      | U.Decl.Str (x, n, sgn) => #1 (E.pushStrNamed env x sgn)
                                      | U.Decl.Sgn (x, n, sgn) => #1 (E.pushSgnNamed env x sgn)
                        }
                        env
                        { smallestgoodpart = NONE
                        , smallest = { item = Decl (#1 modDecl, { file = file
                                                                , first = { line = 0, char = 0}
                                                                , last = { line = 99999, char = 0} })
                                     , span = { file = file
                                              , first = { line = 0, char = 0}
                                              , last = { line = 99999, char = 0} }
                                     , env = env }
                        }
                        modDecl
            in
                case #smallestgoodpart result of
                    NONE => printLiterally (#smallest result)
                  | SOME (desc, span) => desc
            end

fun getInfo loc =
    case String.tokens (fn ch => ch = #":") loc of
        file :: rowStr :: colStr :: nil =>
        (case (Int.fromString rowStr, Int.fromString colStr) of
             (SOME row, SOME col) => getInfo' file row col
           | _ => P.PD.string "ERROR: Wrong typeOf input format, should be <file:row:col>")
      | _ => P.PD.string "ERROR: Wrong typeOf input format, should be <file:row:col>"
end
