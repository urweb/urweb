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

(* Remove nested function definitions *)

structure Unnest :> UNNEST = struct

open Elab

structure E = ElabEnv
structure U = ElabUtil

structure IS = IntBinarySet

val fvsCon = U.Con.foldB {kind = fn (_, st) => st,
                          con = fn (cb, c, cvs) =>
                                   case c of
                                       CRel n =>
                                       if n >= cb then
                                           IS.add (cvs, n - cb)
                                       else
                                           cvs
                                     | _ => cvs,
                          bind = fn (cb, b) =>
                                    case b of
                                        U.Con.Rel _ => cb + 1
                                      | _ => cb}
                         0 IS.empty

fun fvsExp nr = U.Exp.foldB {kind = fn (_, st) => st,
                             con = fn ((cb, eb), c, st as (cvs, evs)) =>
                                      case c of
                                          CRel n =>
                                          if n >= cb then
                                              (IS.add (cvs, n - cb), evs)
                                          else
                                              st
                                        | _ => st,
                             exp = fn ((cb, eb), e, st as (cvs, evs)) =>
                                      case e of
                                          ERel n =>
                                          if n >= eb then
                                              (cvs, IS.add (evs, n - eb))
                                          else
                                              st
                                        | _ => st,
                             bind = fn (ctx as (cb, eb), b) =>
                                       case b of
                                           U.Exp.RelC _ => (cb + 1, eb)
                                         | U.Exp.RelE _ => (cb, eb + 1)
                                         | _ => ctx}
                            (0, nr) (IS.empty, IS.empty)

fun positionOf (x : int) ls =
    let
        fun po n ls =
            case ls of
                [] => raise Fail "Unnest.positionOf"
              | x' :: ls' =>
                if x' = x then
                    n
                else
                    po (n + 1) ls'
    in
        po 0 ls
        handle Fail _ => raise Fail ("Unnset.positionOf("
                                     ^ Int.toString x
                                     ^ ", "
                                     ^ String.concatWith ";" (map Int.toString ls)
                                     ^ ")")
    end

fun squishCon cfv =
    U.Con.mapB {kind = fn k => k,
                con = fn cb => fn c =>
                                  case c of
                                      CRel n =>
                                      if n >= cb then
                                          CRel (positionOf (n - cb) cfv + cb)
                                      else
                                          c
                                    | _ => c,
                bind = fn (cb, b) =>
                          case b of
                              U.Con.Rel _ => cb + 1
                            | _ => cb}
               0

fun squishExp (nr, cfv, efv) =
    U.Exp.mapB {kind = fn k => k,
                con = fn (cb, eb) => fn c =>
                                        case c of
                                            CRel n =>
                                            if n >= cb then
                                                CRel (positionOf (n - cb) cfv + cb)
                                            else
                                                c
                                          | _ => c,
                exp = fn (cb, eb) => fn e =>
                                        case e of
                                            ERel n =>
                                            if n >= eb then
                                                 ERel (positionOf (n - eb) efv + eb)
                                            else
                                                e
                                          | _ => e,
                bind = fn (ctx as (cb, eb), b) =>
                          case b of
                              U.Exp.RelC _ => (cb + 1, eb)
                            | U.Exp.RelE _ => (cb, eb + 1)
                            | _ => ctx}
               (0, nr)

type state = {
     maxName : int,
     decls : decl list
}

fun kind (k, st) = (k, st)

fun exp ((ks, ts), e, st : state) =
    case e of
        ELet (eds, e) =>
        let
            val doSubst = foldl (fn (p, e) => E.subExpInExp p e)

            val (eds, (maxName, ds, subs)) =
                ListUtil.foldlMapConcat
                (fn (ed, (maxName, ds, subs)) =>
                    case #1 ed of
                        EDVal _ => ([ed], (maxName, ds, map (fn (n, e) => (n + 1, E.liftExpInExp 0 e)) subs))
                      | EDValRec vis =>
                        let
                            val loc = #2 ed

                            val nr = length vis
                            val (cfv, efv) = foldl (fn ((_, t, e), (cfv, efv)) =>
                                                       let
                                                           val (cfv', efv') = fvsExp nr e
                                                           (*val () = Print.prefaces "fvsExp"
                                                                    [("e", ElabPrint.p_exp E.empty e),
                                                                     ("cfv", Print.PD.string
                                                                                 (Int.toString (IS.numItems cfv'))),
                                                                     ("efv", Print.PD.string
                                                                                 (Int.toString (IS.numItems efv')))]*)
                                                           val cfv'' = fvsCon t
                                                       in
                                                           (IS.union (cfv, IS.union (cfv', cfv'')),
                                                            IS.union (efv, efv'))
                                                       end)
                                                   (IS.empty, IS.empty) vis

                            (*val () = print ("A: " ^ Int.toString (length ts) ^ ", " ^ Int.toString (length ks) ^ "\n")*)
                            val cfv = IS.foldl (fn (x, cfv) =>
                                                   let
                                                       (*val () = print (Int.toString x ^ "\n")*)
                                                       val (_, t) = List.nth (ts, x)
                                                   in
                                                       IS.union (cfv, fvsCon t)
                                                   end)
                                               cfv efv
                            (*val () = print "B\n"*)

                            val (vis, maxName) =
                                ListUtil.foldlMap (fn ((x, t, e), maxName) =>
                                                      ((x, maxName, t, e),
                                                       maxName + 1))
                                maxName vis

                            fun apply e =
                                let
                                    val e = IS.foldl (fn (x, e) =>
                                                         (ECApp (e, (CRel x, loc)), loc))
                                            e cfv
                                in
                                    IS.foldl (fn (x, e) =>
                                                 (EApp (e, (ERel x, loc)), loc))
                                             e efv
                                end

                            val subs = map (fn (n, e) => (n + nr, E.liftExpInExp nr e)) subs

                            val subs' = ListUtil.mapi (fn (i, (_, n, _, _)) =>
                                                          let
                                                              val dummy = (EError, ErrorMsg.dummySpan)
                                                                          
                                                              fun repeatLift k =
                                                                  if k = 0 then
                                                                      apply (ENamed n, loc)
                                                                  else
                                                                      E.liftExpInExp 0 (repeatLift (k - 1))
                                                          in
                                                              (0, repeatLift i)
                                                          end)
                                                      vis

                            val subs' = rev subs'

                            val cfv = IS.listItems cfv
                            val efv = IS.listItems efv
                            val efn = length efv

                            val subs = subs @ subs'

                            val vis = map (fn (x, n, t, e) =>
                                              let
                                                  (*val () = Print.prefaces "preSubst"
                                                                          [("e", ElabPrint.p_exp E.empty e)]*)
                                                  val e = doSubst e subs

                                                  (*val () = Print.prefaces "squishCon"
                                                                          [("t", ElabPrint.p_con E.empty t)]*)
                                                  val t = squishCon cfv t
                                                  (*val () = Print.prefaces "squishExp"
                                                                          [("e", ElabPrint.p_exp E.empty e)]*)
                                                  val e = squishExp (nr, cfv, efv) e

                                                  val (e, t) = foldr (fn (ex, (e, t)) =>
                                                                         let
                                                                             val (name, t') = List.nth (ts, ex)
                                                                         in
                                                                             ((EAbs (name,
                                                                                     t',
                                                                                     t,
                                                                                     e), loc),
                                                                              (TFun (t',
                                                                                     t), loc))
                                                                         end)
                                                                     (e, t) efv

                                                  val (e, t) = foldr (fn (cx, (e, t)) =>
                                                                         let
                                                                             val (name, k) = List.nth (ks, cx)
                                                                         in
                                                                             ((ECAbs (Explicit,
                                                                                      name,
                                                                                      k,
                                                                                      e), loc),
                                                                              (TCFun (Explicit,
                                                                                      name,
                                                                                      k,
                                                                                      t), loc))
                                                                         end)
                                                                     (e, t) cfv
                                              in
                                                  (x, n, t, e)
                                              end)
                                          vis

                            val d = (DValRec vis, #2 ed)
                        in
                            ([], (maxName, d :: ds, subs))
                        end)
                (#maxName st, #decls st, []) eds
        in
            (ELet (eds, doSubst e subs),
             {maxName = maxName,
              decls = ds})
        end

      | _ => (e, st)

fun default (ctx, d, st) = (d, st)

fun bind ((ks, ts), b) =
    case b of
        U.Decl.RelC p => (p :: ks, map (fn (name, t) => (name, E.liftConInCon 0 t)) ts)
      | U.Decl.RelE p => (ks, p :: ts)
      | _ => (ks, ts)                        

val unnestDecl = U.Decl.foldMapB {kind = kind,
                                  con = default,
                                  exp = exp,
                                  sgn_item = default,
                                  sgn = default,
                                  str = default,
                                  decl = default,
                                  bind = bind}
                                 ([], [])

fun unnest file =
    let
        fun doDecl (all as (d, loc), st : state) =
            let
                fun default () = ([all], st)
                fun explore () =
                    let
                        val (d, st) = unnestDecl st all
                    in
                        (rev (d :: #decls st),
                         {maxName = #maxName st,
                          decls = []})
                    end
            in
                case d of
                    DCon _ => default ()
                  | DDatatype _ => default ()
                  | DDatatypeImp _ => default ()
                  | DVal _ => explore ()
                  | DValRec _ => explore ()
                  | DSgn _ => default ()
                  | DStr (x, n, sgn, str) =>
                    let
                        val (str, st) = doStr (str, st)
                    in
                        ([(DStr (x, n, sgn, str), loc)], st)
                    end
                  | DFfiStr _ => default ()
                  | DConstraint _ => default ()
                  | DExport _ => default ()
                  | DTable _ => default ()
                  | DSequence _ => default ()
                  | DClass _ => default ()
                  | DDatabase _ => default ()
            end

        and doStr (all as (str, loc), st) =
            let
                fun default () = (all, st)
            in
                case str of
                    StrConst ds =>
                    let
                        val (ds, st) = ListUtil.foldlMapConcat doDecl st ds
                    in
                        ((StrConst ds, loc), st)
                    end
                  | StrVar _ => default ()
                  | StrProj _ => default ()
                  | StrFun (x, n, dom, ran, str) =>
                    let
                        val (str, st) = doStr (str, st)
                    in
                        ((StrFun (x, n, dom, ran, str), loc), st)
                    end
                  | StrApp _ => default ()
                  | StrError => raise Fail "Unnest: StrError"
            end

        val (ds, _) = ListUtil.foldlMapConcat doDecl
                      {maxName = U.File.maxName file + 1,
                       decls = []} file
    in
        ds
    end

end
