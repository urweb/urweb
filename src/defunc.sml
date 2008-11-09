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

structure Defunc :> DEFUNC = struct

open Core

structure E = CoreEnv
structure U = CoreUtil

structure IS = IntBinarySet

val functionInside = U.Con.exists {kind = fn _ => false,
                                   con = fn TFun _ => true
                                          | CFfi ("Basis", "transaction") => true
                                          | _ => false}

val freeVars = U.Exp.foldB {kind = fn (_, xs) => xs,
                            con = fn (_, _, xs) => xs,
                            exp = fn (bound, e, xs) =>
                                     case e of
                                         ERel x =>
                                         if x >= bound then
                                             IS.add (xs, x - bound)
                                         else
                                             xs
                                       | _ => xs,
                            bind = fn (bound, b) =>
                                      case b of
                                          U.Exp.RelE _ => bound + 1
                                        | _ => bound}
                           0 IS.empty

fun positionOf (v : int, ls) =
    let
        fun pof (pos, ls) =
            case ls of
                [] => raise Fail "Defunc.positionOf"
              | v' :: ls' =>
                if v = v' then
                    pos
                else
                    pof (pos + 1, ls')
    in
        pof (0, ls)
    end

fun squish fvs =
    U.Exp.mapB {kind = fn k => k,
                con = fn _ => fn c => c,
                exp = fn bound => fn e =>
                                     case e of
                                         ERel x =>
                                         if x >= bound then
                                             ERel (positionOf (x - bound, fvs) + bound)
                                         else
                                             e
                                       | _ => e,
                bind = fn (bound, b) =>
                          case b of
                              U.Exp.RelE _ => bound + 1
                            | _ => bound}
               0

fun default (_, x, st) = (x, st)

datatype 'a search =
         Yes
       | No
       | Maybe of 'a

structure EK = struct
type ord_key = exp
val compare = U.Exp.compare
end

structure EM = BinaryMapFn(EK)

type state = {
     maxName : int,
     funcs : int EM.map,
     vis : (string * int * con * exp * string) list
}

fun exp (env, e, st) =
    case e of
        ERecord xes =>
        let
            val (xes, st) =
                ListUtil.foldlMap
                    (fn (tup as (fnam as (CName x, loc), e, xt), st) =>
                         if (x <> "Link" andalso x <> "Action")
                            orelse case #1 e of
                                       ENamed _ => true
                                     | _ => false then
                             (tup, st)
                         else
                             let
                                 fun needsAttention (e, _) =
                                     case e of
                                         ENamed f => Maybe (#2 (E.lookupENamed env f))
                                       | EApp (f, _) =>
                                         (case needsAttention f of
                                              No => No
                                            | Yes => Yes
                                            | Maybe t =>
                                              case t of
                                                  (TFun (dom, _), _) =>
                                                  if functionInside dom then
                                                      Yes
                                                  else
                                                      No
                                                | _ => No)
                                       | _ => No

                                 fun headSymbol (e, _) =
                                     case e of
                                         ENamed f => f
                                       | EApp (e, _) => headSymbol e
                                       | _ => raise Fail "Defunc: headSymbol"

                                 fun rtype (e, _) =
                                     case e of
                                         ENamed f => #2 (E.lookupENamed env f)
                                       | EApp (f, _) =>
                                         (case rtype f of
                                              (TFun (_, ran), _) => ran
                                            | _ => raise Fail "Defunc: rtype [1]")
                                       | _ => raise Fail "Defunc: rtype [2]"
                             in
                                 (*Print.prefaces "Found one!"
                                                [("e", CorePrint.p_exp env e)];*)
                                 case needsAttention e of
                                     Yes =>
                                     let
                                         (*val () = print "Yes\n"*)
                                         val f = headSymbol e

                                         val fvs = IS.listItems (freeVars e)

                                         val e = squish fvs e
                                         val (e, t) = foldl (fn (n, (e, t)) =>
                                                                let
                                                                    val (x, xt) = E.lookupERel env n
                                                                in
                                                                    ((EAbs (x, xt, t, e), loc),
                                                                     (TFun (xt, t), loc))
                                                                end)
                                                            (e, rtype e) fvs

                                         val (f', st) =
                                             case EM.find (#funcs st, e) of
                                                 SOME f' => (f', st)
                                               | NONE =>
                                                 let
                                                     val (fx, _, _, tag) = E.lookupENamed env f
                                                     val f' = #maxName st

                                                     val vi = (fx, f', t, e, tag)
                                                 in
                                                     (f', {maxName = f' + 1,
                                                           funcs = EM.insert (#funcs st, e, f'),
                                                           vis = vi :: #vis st})
                                                 end

                                         val e = foldr (fn (n, e) =>
                                                           (EApp (e, (ERel n, loc)), loc))
                                                       (ENamed f', loc) fvs
                                     in
                                         (*app (fn n => Print.prefaces
                                                            "Free"
                                                            [("n", CorePrint.p_exp env (ERel n, ErrorMsg.dummySpan))])
                                               fvs;
                                          Print.prefaces "Squished"
                                                         [("e", CorePrint.p_exp CoreEnv.empty e)];*)

                                         ((fnam, e, xt), st)
                                     end
                                   | _ => (tup, st)
                             end
                      | (tup, st) => (tup, st))
                st xes
        in
            (ERecord xes, st)
        end
      | _ => (e, st)

fun bind (env, b) =
    case b of
        U.Decl.RelC (x, k) => E.pushCRel env x k
      | U.Decl.NamedC (x, n, k, co) => E.pushCNamed env x n k co
      | U.Decl.RelE (x, t) => E.pushERel env x t
      | U.Decl.NamedE (x, n, t, eo, s) => E.pushENamed env x n t eo s

fun doDecl env = U.Decl.foldMapB {kind = fn x => x,
                                  con = default,
                                  exp = exp,
                                  decl = default,
                                  bind = bind}
                                 env

fun defunc file =
    let
        fun doDecl' (d, (env, st)) =
            let
                val env = E.declBinds env d

                val (d, st) = doDecl env st d

                val ds =
                    case #vis st of
                        [] => [d]
                      | vis =>
                        case d of
                            (DValRec vis', loc) => [(DValRec (vis' @ vis), loc)]
                          | _ => [(DValRec vis, #2 d), d]
            in
                (ds,
                 (env,
                  {maxName = #maxName st,
                   funcs = #funcs st,
                   vis = []}))
            end

        val (file, _) = ListUtil.foldlMapConcat doDecl'
                        (E.empty,
                         {maxName = U.File.maxName file + 1,
                          funcs = EM.empty,
                          vis = []})
                        file
    in
        file
    end

end
