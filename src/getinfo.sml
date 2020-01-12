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

fun isPosIn (file: string) (line: int) (char: int) (span: ErrorMsg.span) =
    let
        val start = #first span
        val end_ = #last span
    in
        OS.Path.base file = OS.Path.base (#file span)
        andalso
        (#line start < line orelse
         #line start = line andalso #char start <= char)
        andalso 
        (#line end_ > line orelse
         #line end_ = line andalso #char end_ >= char)
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

fun getSpan (f: item) =
    case f of
        Kind k => #2 k
      | Con c => #2 c
      | Exp e => #2 e
      | Sgn_item si => #2 si
      | Sgn s => #2 s
      | Str s => #2 s
      | Decl d => #2 d


fun findInStr (f: ElabEnv.env -> item (* curr *) -> item (* prev *) -> bool)
              (init: item)
              env str fileName {line = line, char = char}: {item: item, env: ElabEnv.env} =
    let
        val () = U.mliftConInCon := E.mliftConInCon
        val {env: ElabEnv.env, found: Elab.decl option} =
            (case str of
                 L.StrConst decls =>
                  List.foldl (fn (d, acc as {env, found}) =>
                                if #line (#last (#2 d)) < line
                                then {env = E.declBinds env d, found = found}
                                else
                                    if #line (#first (#2 d)) <= line andalso line <= #line (#last (#2 d))
                                    then {env = env, found = SOME d}
                                    else {env = env, found = found})
                            {env = env, found = NONE} decls
               | _ => { env = env, found = NONE })
        val dummyResult = (init, env)
        val result =
            case found of
                NONE => dummyResult
              | SOME d => 
                U.Decl.foldB
                    { kind = fn (env, i, acc as (prev, env')) => if f env (Kind i) prev then (Kind i, env) else acc,
                      con = fn (env, i, acc as (prev, env')) => if f env (Con i) prev then (Con i, env) else acc,
                      exp = fn (env, i, acc as (prev, env')) => if f env (Exp i) prev then (Exp i, env) else acc,
                      sgn_item = fn (env, i, acc as (prev, env')) => if f env (Sgn_item i) prev then (Sgn_item i, env) else acc,
                      sgn = fn (env, i, acc as (prev, env')) => if f env (Sgn i) prev then (Sgn i, env) else acc,
                      str = fn (env, i, acc as (prev, env')) => if f env (Str i) prev then (Str i, env) else acc,
                      decl = fn (env, i, acc as (prev, env')) => if f env (Decl i) prev then (Decl i, env) else acc,
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
                    env dummyResult d
    in
        {item = #1 result, env = #2 result}
    end

fun findClosestSpan env str fileName {line = line, char = char} =
    let
        fun getDistance (i: item): int =
            let
                val {first, last, file} = getSpan i
            in
                Int.abs (#char first - char)
                + Int.abs (#char last - char)
                + Int.abs (#line first - line) * 25
                + Int.abs (#line last - line) * 25
            end
        fun isCloser (env: ElabEnv.env) (curr: item) (prev: item) =
            getDistance curr < getDistance prev
        val init = Str (str, { file = fileName
                             , first = { line = 0, char = 0}
                             , last = { line = 0, char = 0} })
    in
        findInStr isCloser init env str fileName {line = line, char = char}
    end

fun findFirstExpAfter env str fileName {line = line, char = char} =
    let
        fun currIsAfterPosAndBeforePrev (env: ElabEnv.env) (curr: item) (prev: item) =
            (* curr is an exp *)
            (case curr of Exp _ => true | _ => false)
            andalso
            (* curr is after input pos *)
            ( line < #line (#first (getSpan curr))
              orelse ( line = #line (#first (getSpan curr))
                       andalso char < #char (#first (getSpan curr))))
            andalso
            (* curr is before prev *)
            (#line (#first (getSpan curr)) < #line (#first (getSpan prev))
             orelse
             (#line (#first (getSpan curr)) = #line (#first (getSpan prev))
              andalso #char (#first (getSpan curr)) < #char (#first (getSpan prev))))
        val init = Exp (Elab.EPrim (Prim.Int 0),
                        { file = fileName
                        , first = { line = Option.getOpt (Int.maxInt, 99999), char = Option.getOpt (Int.maxInt, 99999)}
                        , last = { line = Option.getOpt (Int.maxInt, 99999), char = Option.getOpt (Int.maxInt, 99999)} })
    in
        findInStr currIsAfterPosAndBeforePrev init env str fileName {line = line, char = char}
    end


datatype foundInEnv = FoundStr of (string * Elab.sgn)
                    | FoundKind of (string * Elab.kind)
                    | FoundCon of (string * Elab.con)

fun getNameOfFoundInEnv (f: foundInEnv) =
    case f of
        FoundStr (x, _) => x
      | FoundKind (x, _) => x
      | FoundCon (x, _) => x

fun filterSgiItems (items: Elab.sgn_item list) : foundInEnv list =
    let
        fun processDatatype loc (dtx, i, ks, cs) =
            let
                val k' = (Elab.KType, loc)
                val k = FoundKind (dtx, foldl (fn (_, k) => (Elab.KArrow (k', k), loc)) k' ks)
                val foundCs = List.map (fn (x, j, co) =>
                                           let
                                               val c = case co of
                                                           NONE => (Elab.CNamed i, loc)
                                                         | SOME c => (Elab.TFun (c, (Elab.CNamed i, loc)), loc)
                                           in
                                               FoundCon (x, c)
                                           end) cs
            in
                k :: foundCs
            end
        fun mapF item =
            case item of
                (Elab.SgiVal (name, _, c), _) => [FoundCon (name, c)]
              | (Elab.SgiCon (name, _, k, _), _) => [FoundKind (name, k)]
              | (Elab.SgiDatatype ds, loc) =>
                List.concat (List.map (processDatatype loc) ds)
              | (Elab.SgiDatatypeImp (dtx, i, _, ks, _, _, cs), loc) => processDatatype loc (dtx, i, ks, cs)
              | (Elab.SgiStr (_, name,  _, sgn), _) =>
                [FoundStr (name, sgn)]
              | (Elab.SgiSgn (name,  _, sgn), _) => []
              | _ => []
    in
        List.concat (List.map mapF items)
    end

fun resolvePrefixes
    (env: ElabEnv.env)
    (prefixes: string list)
    (items : foundInEnv list)
    : foundInEnv list 
    = 
    case prefixes of
        [] => items
      | first :: rest => 
        (case List.find (fn item => getNameOfFoundInEnv item = first) items of
             NONE => []
           | SOME (FoundStr (name, sgn)) => (case ElabEnv.hnormSgn env sgn of
                                                 (Elab.SgnConst sgis, _) => resolvePrefixes env rest (filterSgiItems sgis)
                                               | _ => [])
           | SOME (FoundCon (name, c)) =>
             let
                 val fields = case ElabOps.reduceCon env c of
                                  (Elab.TRecord (Elab.CRecord (_, fields), l2_), l1_) =>
                                  fields
                                | ( ( Elab.CApp
                                      ( ( (Elab.CApp
                                            ( ( Elab.CModProj (_, _, "sql_table") , l4_)
                                            , ( Elab.CRecord (_, fields) , l3_)))
                                        , l2_)
                                      , _))
                                  , l1_) => fields
                                | _ => []
                 val items = 
                     List.mapPartial (fn (c1, c2) => case c1 of
                                                         (Elab.CName fieldName, _) => SOME (FoundCon (fieldName, c2))
                                                       | _  => NONE) fields
             in
                 resolvePrefixes env rest items
             end
           | SOME (FoundKind (_, _)) => [])
                                         

fun findStringInEnv' (env: ElabEnv.env) (preferCon: bool) (str: string): (string (* prefix *) * foundInEnv option) =
    let
        val splitted = List.map Substring.string (Substring.fields (fn c => c = #".") (Substring.full str))
        val afterResolve = resolvePrefixes env (List.take (splitted, List.length splitted - 1))
                                                ( List.map (fn (name, (_, sgn)) => FoundStr (name, sgn)) (ElabEnv.dumpStrs env)
                                                @ List.map FoundKind (ElabEnv.dumpCs env)
                                                @ List.map FoundCon (ElabEnv.dumpEs env))
        val query = List.last splitted 
        val prefix = String.extract (str, 0, SOME (String.size str - String.size query))
    in
        (prefix, List.find (fn i => getNameOfFoundInEnv i = query) afterResolve)
    end

fun matchStringInEnv' (env: ElabEnv.env) (str: string): (string (* prefix *) * foundInEnv list) =
    let
        val splitted = List.map Substring.string (Substring.fields (fn c => c = #".") (Substring.full str))
        val afterResolve = resolvePrefixes env (List.take (splitted, List.length splitted - 1))
                                                ( List.map (fn (name, (_, sgn)) => FoundStr (name, sgn)) (ElabEnv.dumpStrs env)
                                                  @ List.map FoundKind (ElabEnv.dumpCs env)
                                                  @ List.map FoundCon (ElabEnv.dumpEs env))
        val query = List.last splitted 
        val prefix = String.extract (str, 0, SOME (String.size str - String.size query))
    in
        (prefix, List.filter (fn i => String.isPrefix query (getNameOfFoundInEnv i)) afterResolve)
    end

fun getDesc item =
    case item of
        Kind (_, s) => "Kind " ^ ErrorMsg.spanToString s
      | Con (_, s) =>  "Con " ^ ErrorMsg.spanToString s
      | Exp (_, s) =>  "Exp " ^ ErrorMsg.spanToString s
      | Sgn_item (_, s) =>  "Sgn_item " ^ ErrorMsg.spanToString s
      | Sgn (_, s) =>  "Sgn " ^ ErrorMsg.spanToString s
      | Str (_, s) =>  "Str " ^ ErrorMsg.spanToString s
      | Decl (_, s) =>  "Decl " ^ ErrorMsg.spanToString s

fun matchStringInEnv env str fileName pos query: (ElabEnv.env * string (* prefix *) * foundInEnv list) = 
    let
        val {item = _, env} = findClosestSpan env str fileName pos
        val (prefix, matches) = matchStringInEnv' env query
    in
        (env, prefix, matches)
    end

fun findStringInEnv env str fileName pos (query: string): (ElabEnv.env * string (* prefix *) * foundInEnv option) =
    let
        val {item, env} = findClosestSpan env str fileName pos
        val env = case item of
                      Exp (L.ECase _, _) => #env (findFirstExpAfter env str fileName pos)
                    | Exp (L.ELet _, _)  => #env (findFirstExpAfter env str fileName pos)
                    | Exp (L.EAbs _, _)  => #env (findFirstExpAfter env str fileName pos)
                    | Exp e              => env
                    | Con _              => #env (findFirstExpAfter env str fileName pos)
                    | _                  => #env (findFirstExpAfter env str fileName pos)
        val preferCon = case item of Con _ => true 
                                   | _ => false
        val (prefix, found) = findStringInEnv' env preferCon query
    in
        (env, prefix, found)
    end
end
