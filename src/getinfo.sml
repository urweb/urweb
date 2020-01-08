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

fun isPosIn (file: string) (row: int) (col: int) (span: ErrorMsg.span) =
    let
        val start = #first span
        val end_ = #last span
    in
        OS.Path.base file = OS.Path.base (#file span)
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

(* Just use ElabPrint functions. *)
(* These are better for compiler error messages, but it's better than nothing *)
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

fun getInfo env str fileName {line = row, character = col} =
    let
        val () = U.mliftConInCon := E.mliftConInCon

        (* Adding previous declarations in file to environment *)
        (* "open <mod>" statements are already translated during elaboration *)
        (*   They get added to the env here "unprefixed" *)
        val env = (case str of
                       L.StrConst decls =>
                       List.foldl (fn (d, env) =>
                                      if #line (#first (#2 d)) <= row
                                         andalso #char (#first (#2 d)) <= col
                                      then E.declBinds env d
                                      else env) env decls
                     | _ => env)

        (* This isn't very precise since we use the span of the parent exp/decl/etc *)
        (* to find the "smallest part" *)
        fun printPat env (pat: L.pat) =
            if isPosIn fileName row col (#2 pat)
            then
                case #1 pat of
                    L.PVar (str, c) => SOME (P.box [ P.PD.string str
                                                   , P.PD.string " : "
                                                   , ElabPrint.p_con env c])
                  | L.PCon (_, _, _, SOME p) => printPat env p
                  | L.PRecord fields => (case List.mapPartial (fn field => printPat env (#2 field)) fields of
                                             [] => NONE
                                           | first :: _ => SOME first)
                  | _ => NONE
            else NONE

        fun isXmlTag env c = 
            case c of
                L.CApp
                    ((L.CApp
                          ((L.CApp
                                (( L.CApp
                                       (( L.CApp
                                              ((L.CNamed n, _) , _)
                                        , _)
                                       , _)
                                 , _)
                                , _)
                           , _)
                          , _)
                     , _)
                    , _) =>
                (case E.lookupCNamed env n of
                     ("tag", _, _) => true
                   | _ => false)
              | _ => false

        fun formatTypeBox (a: P.PD.pp_desc, b: P.PD.pp_desc) =
            P.PD.hvBox (P.PD.PPS.Rel 0, [a,
                                         P.PD.string ": ",
                                         P.PD.break {nsp = 0, offset = 2},
                                         b])
                         
        (* TODO We lose some really useful information, like eg. inferred parameters, *)
        (*   which we do have in the actual items (L.Decl, L.Exp, etc) *)
        (*   but not when we do a lookup into the Env *)
        (* TODO Rename? *)
        fun printGoodPart env f span =
            (case f of
                  Exp (L.EPrim p, _) =>
                  let
                      val rendered = formatTypeBox ( Prim.p_t p
                                                   , P.PD.string (case p of
                                                                      Prim.Int _ => "int"
                                                                    | Prim.Float _ => "float"
                                                                    | Prim.String _ => "string"
                                                                    | Prim.Char _ => "char"))
                  in
                      case p of
                          Prim.String (_, str) =>
                          if Substring.foldl (fn (c, acc) => acc andalso c = #" ") true (Substring.full str)
                          then NONE
                          else SOME rendered
                        | _ => SOME (rendered)
                  end
                | Exp (L.ERel n, _) =>
                  SOME ((let val found = E.lookupERel env n
                        in
                            formatTypeBox ( P.PD.string (#1 found)
                                          , ElabPrint.p_con env (#2 found))
                        end)
                        handle E.UnboundRel _ => P.PD.string ("UNBOUND_REL" ^ Int.toString n))
                | Exp (L.ENamed n, span) =>
                  ((let
                      val found = E.lookupENamed env n
                      val rendered = formatTypeBox ( P.PD.string (#1 found)
                                                   , ElabPrint.p_con env (#2 found))
                      (* val () = if #1 found = "body" *)
                      (*           then Print.eprint (ElabPrint.p_con env (#2 found)) *)
                      (*           else () *)
                  in
                        (* case #2 found of *)
                        (*     (L.TFun ((L.CUnit, _), (c, _)), _) => *)
                        (*     (if isXmlTag env c *)
                        (*     then SOME (P.box [ P.PD.string "<" *)
                        (*                       , P.PD.string ( #1 found) *)
                        (*                       , P.PD.string ">" *)
                        (*               ]) *)
                        (*     else SOME rendered) *)
                        (* | _ => *) SOME rendered
                    end)
                  handle E.UnboundNamed _ => SOME (P.PD.string ("UNBOUND_NAMED" ^ Int.toString n)))
                | Exp (L.EAbs (varName, domain, _, _), _) => 
                  if isPosIn fileName row col (#2 domain)
                  then
                      SOME (formatTypeBox ( P.PD.string varName
                                          , ElabPrint.p_con env domain)
                           )
                  else NONE
                | Exp (L.EField (e, c, {field, ...}), loc) => 
                  SOME (formatTypeBox ( P.box [ElabPrint.p_exp env e,
                                               P.PD.string ".",
                                               ElabPrint.p_con env c]
                                      , ElabPrint.p_con env field))
                | Exp (L.EModProj ( m1 (* number (= "name") of top level module *)
                                  , ms (* names of submodules - possibly none *)
                                  , x (* identifier *)), loc) =>
                  (let
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
                      case (m1name, x) of
                          (* Stripping these because XML desugaring adds these with small spans and crowd out the stuff you want to see *)
                          ("Basis", "cdata") => NONE
                        | ("Top", "txt") => NONE
                        | ("Basis", "join") => NONE
                        | ("Basis", "bind") => NONE
                        | ("Basis", "sql_subset") => NONE
                        | ("Basis", "sql_subset_all") => NONE
                        | ("Basis", "sql_query") => NONE
                        | ("Basis", "sql_query1") => NONE
                        | ("Basis", "sql_eq") => NONE
                        | ("Basis", "sql_inner_join") => NONE
                        (* | ("Basis", "sql_field") => NONE *)
                        | ("Basis", "sql_binary") => NONE
                        | _ => 
                          SOME (formatTypeBox ( P.p_list_sep (P.PD.string ".") P.PD.string (m1name :: ms @ [x])
                                              , ElabPrint.p_con env t))
                  end
                  handle E.UnboundNamed _ => SOME (P.PD.string ("Module not found: " ^ Int.toString m1)))
                | Exp (L.ELet (edecls, _, _), _) =>
                  let
                      val found = List.mapPartial
                                      (fn (edecl, loc) =>
                                          if isPosIn fileName row col loc
                                          then
                                              case edecl of
                                                  L.EDVal (pat, _, _) => printPat env pat
                                                | L.EDValRec ((x, c, _) :: _) =>
                                                  SOME (formatTypeBox ( P.PD.string x
                                                                      , ElabPrint.p_con env c))
                                                | _ => NONE
                                          else NONE)
                                      edecls 
                  in
                      if List.length found > 0
                      then SOME (List.hd found)
                      else NONE
                  end
                | Exp (L.ECase (_, pats, _), _) => 
                  (case List.find (fn ((pat', loc), exp) => isPosIn fileName row col loc) pats of
                      NONE => NONE
                    | SOME (pat, _) => printPat env pat)
                | Exp e => NONE
                | Kind k => NONE
                | Con c => NONE
                | Sgn_item si => NONE
                | Sgn s => NONE
                | Str s => NONE
                | Decl (L.DVal (x, _, con, _), _) =>
                  SOME (formatTypeBox ( P.PD.string x
                                      , ElabPrint.p_con env con ))
                | Decl (L.DValRec decls, _) =>
                  (* valrecs don't have nice spans per declaration so we find the *)
                  (* declaration for which the con starts closest *)
                  let
                      val res = 
                          List.foldl (fn (decl, accO) =>
                                         let
                                             val distanceFromRow = Int.abs (#line (#first (#2 (#3 decl))) - row)
                                             val accDistanceFromRow = case accO of
                                                                          NONE => Option.getOpt (Int.maxInt, 99999)
                                                                        | SOME acc => Int.abs (#line (#first (#2 (#3 acc))) - row)
                                         in
                                             if distanceFromRow < accDistanceFromRow andalso distanceFromRow <= 1
                                             then SOME decl
                                             else accO
                                         end)
                                    NONE
                                    decls
                  in
                      case res of
                          NONE => NONE
                        | SOME (x, _, con, _) =>
                          SOME (formatTypeBox ( P.PD.string x
                                              , ElabPrint.p_con env con))
                  end
                | Decl d => NONE
            )

        fun add (env: ElabEnv.env) (item: item) (span: ErrorMsg.span) (acc: { smallest : { span : ErrorMsg.span
                                                                                         , item : item
                                                                                         , env : ElabEnv.env }
                                                                            , smallestgoodpart : { span : ErrorMsg.span
                                                                                                 , desc : P.PD.pp_desc
                                                                                                 , env : ElabEnv.env
                                                                                                 , item : item
                                                                                                 } option
                                                                            }
                                                                      ) =
                    if not (isPosIn fileName row col span)
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
                                      | SOME desc => SOME {desc = desc, span = span, env = env, item = item})
                                  | SOME (prev as {desc = desc', span = span', env = env', item = item'}) => 
                                    if
                                        isSmallerThan span span'
                                    then 
                                        (case printGoodPart env item span of
                                             NONE => SOME prev
                                          | SOME desc => SOME {desc = desc, span = span, env = env, item = item})
                                    else SOME prev
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
                , smallest = { item = Str (str, { file = fileName
                                                , first = { line = 0, char = 0}
                                                , last = { line = Option.getOpt (Int.maxInt, 99999), char = 0} })
                             , span = { file = fileName
                                      , first = { line = 0, char = 0}
                                      , last = { line = Option.getOpt (Int.maxInt, 99999), char = 0} }
                             , env = env }
                }
                ( L.DStr (Compiler.moduleOf "fileName", 0, (L.SgnError, ErrorMsg.dummySpan), (str, {file = fileName, first = ErrorMsg.dummyPos, last = ErrorMsg.dummyPos}))
                , {file = fileName, first = ErrorMsg.dummyPos, last = ErrorMsg.dummyPos})
    in
        result
    end
end
