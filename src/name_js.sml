(* Copyright (c) 2012-2013, Adam Chlipala
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

(* Phase that introduces names for fragments of JavaScript code, so that they
 * may be moved to app.js and not repeated in each generated page *)

structure NameJS :> NAME_JS = struct

open Mono

structure U = MonoUtil
structure IS = IntBinarySet

val freeVars = U.Exp.foldB {typ = #2,
                            exp = fn (free, e, vs) =>
                                     case e of
                                         ERel n =>
                                         if n < free then
                                             vs
                                         else
                                             IS.add (vs, n - free)
                                       | _ => vs,
                            bind = fn (free, b) =>
                                      case b of
                                          U.Exp.RelE _ => free+1
                                        | _ => free}
               0 IS.empty

fun index (ls, v) =
    case ls of
        [] => raise Fail "NameJs.index"
      | v' :: ls' => if v = v' then 0 else 1 + index (ls', v)

fun squish vs = U.Exp.mapB {typ = fn x => x,
                            exp = fn free => fn e =>
                                                case e of
                                                    ERel n =>
                                                    if n < free then
                                                        e
                                                    else
                                                        ERel (free + index (vs, n - free) + 1)
                                                  | _ => e,
                            bind = fn (free, b) =>
                                      case b of
                                          U.Exp.RelE _ => free+1
                                        | _ => free}
                           0

fun rewrite file =
    let
        fun isTricky' dontName e =
            case e of
                ENamed n => IS.member (dontName, n)
              | EFfiApp ("Basis", "sigString", _) => true
              | _ => false

        fun isTricky dontName = U.Decl.exists {typ = fn _ => false,
                                               exp = isTricky' dontName,
                                               decl = fn _ => false}

        fun isTrickyE dontName = U.Exp.exists {typ = fn _ => false,
                                               exp = isTricky' dontName}

        val dontName = foldl (fn (d, dontName) =>
                                 if isTricky dontName d then
                                     case #1 d of
                                         DVal (_, n, _, _, _) => IS.add (dontName, n)
                                       | DValRec vis => foldl (fn ((_, n, _, _, _), dontName) => IS.add (dontName, n)) dontName vis
                                       | _ => dontName
                                 else
                                     dontName) IS.empty (#1 file)

        val (ds, _) = ListUtil.foldlMapConcat (fn (d, nextName) =>
                                                    let
                                                        val (d, (nextName, newDs)) =
                                                            U.Decl.foldMapB {typ = fn x => x,
                                                                             decl = fn (_, e, s) => (e, s),
                                                                             exp = fn (env, e, st as (nextName, newDs)) =>
                                                                                      case e of
                                                                                          EJavaScript (mode, e') =>
                                                                                          (case mode of
                                                                                               Source _ => (e, st)
                                                                                             | _ =>
                                                                                               let
                                                                                                   fun isTrulySimple (e, _) =
                                                                                                       case e of
                                                                                                           ERel _ => true
                                                                                                         | ENamed _ => true
                                                                                                         | ERecord [] => true
                                                                                                         | _ => false

                                                                                                   fun isAlreadySimple e =
                                                                                                       case #1 e of
                                                                                                           EApp (e, arg) => isTrulySimple arg andalso isAlreadySimple e
                                                                                                         | _ => isTrulySimple e
                                                                                               in
                                                                                                   if isAlreadySimple e' orelse isTrickyE dontName e' then
                                                                                                       (e, st)
                                                                                                   else
                                                                                                       let
                                                                                                           val loc = #2 e'

                                                                                                           val vs = freeVars e'
                                                                                                           val vs = IS.listItems vs
                                                                                                                    
                                                                                                           val x = "script" ^ Int.toString nextName
                                                                                                                   
                                                                                                           val un = (TRecord [], loc)
                                                                                                           val s = (TFfi ("Basis", "string"), loc)
                                                                                                           val base = (TFun (un, s), loc)
                                                                                                           val t = foldl (fn (n, t) => (TFun (#2 (List.nth (env, n)), t), loc)) base vs
                                                                                                           val e' = squish vs e'
                                                                                                           val e' = (EAbs ("_", un, s, e'), loc)
                                                                                                           val (e', _) = foldl (fn (n, (e', t)) =>
                                                                                                                                   let
                                                                                                                                       val (x, this) = List.nth (env, n)
                                                                                                                                   in
                                                                                                                                       ((EAbs (x, this, t, e'), loc),
                                                                                                                                        (TFun (this, t), loc))
                                                                                                                                   end) (e', base) vs
                                                                                                           val d = (x, nextName, t, e', "<script>")

                                                                                                           val e = (ENamed nextName, loc)
                                                                                                           val e = foldr (fn (n, e) => (EApp (e, (ERel n, loc)), loc)) e vs
                                                                                                           val e = (EApp (e, (ERecord [], loc)), loc)
                                                                                                           val e = EJavaScript (Script, e)
                                                                                                       in
                                                                                                           (e, (nextName+1, d :: newDs))
                                                                                                       end
                                                                                               end)
                                                                                        | _ => (e, st),
                                                                             bind = fn (env, b) =>
                                                                                       case b of
                                                                                           U.Decl.RelE x => x :: env
                                                                                         | _ => env}
                                                            [] (nextName, []) d
                                                    in
                                                        (case newDs of
                                                             [] => [d]
                                                           | _ => case #1 d of
                                                                      DValRec vis => [(DValRec (vis @ newDs), #2 d)]
                                                                    | _ => List.revAppend (map (fn vi => (DVal vi, #2 d)) newDs, [d]),
                                                         nextName)
                                                    end) (U.File.maxName file + 1) (#1 file)
    in
        (ds, #2 file)
    end

end
