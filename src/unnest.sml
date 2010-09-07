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

(* Remove nested function definitions *)

structure Unnest :> UNNEST = struct

open Elab

structure E = ElabEnv
structure U = ElabUtil

structure IS = IntBinarySet

fun liftExpInExp by =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn _ => fn c => c,
                exp = fn bound => fn e =>
                                     case e of
                                         ERel xn =>
                                         if xn < bound then
                                             e
                                         else
                                             ERel (xn + by)
                                       | _ => e,
                bind = fn (bound, U.Exp.RelE _) => bound + 1
                        | (bound, _) => bound}

val subExpInExp =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn _ => fn c => c,
                exp = fn (xn, rep) => fn e =>
                                  case e of
                                      ERel xn' =>
                                      if xn' = xn then
                                           #1 rep
                                      else
                                          e
                                    | _ => e,
                bind = fn ((xn, rep), U.Exp.RelE _) => (xn+1, E.liftExpInExp 0 rep)
                        | ((xn, rep), U.Exp.RelC _) => (xn, E.liftConInExp 0 rep)
                        | (ctx, _) => ctx}

val fvsCon = U.Con.foldB {kind = fn (_, _, st) => st,
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
                                        U.Con.RelC _ => cb + 1
                                      | _ => cb}
                         0 IS.empty

fun fvsExp nr = U.Exp.foldB {kind = fn (_, _, st) => st,
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
        handle Fail _ => raise Fail ("Unnest.positionOf("
                                     ^ Int.toString x
                                     ^ ", "
                                     ^ String.concatWith ";" (map Int.toString ls)
                                     ^ ")")
    end

fun squishCon cfv =
    U.Con.mapB {kind = fn _ => fn k => k,
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
                              U.Con.RelC _ => cb + 1
                            | _ => cb}
               0

fun squishExp (nr, cfv, efv) =
    U.Exp.mapB {kind = fn _ => fn k => k,
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
                                                ERel (positionOf (n - eb) efv + eb - nr)
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
     decls : (string * int * con * exp) list
}

fun kind (_, k, st) = (k, st)

fun exp ((ks, ts), e as old, st : state) =
    case e of
        ELet (eds, e, t) =>
        let
            (*val () = Print.prefaces "Letto" [("e", ElabPrint.p_exp E.empty (old, ErrorMsg.dummySpan))]*)

            fun doSubst' (e, subs) = foldl (fn (p, e) => subExpInExp p e) e subs

            fun doSubst (e, subs, by) =
                let
                    val e = doSubst' (e, subs)
                in
                    liftExpInExp (~by) (length subs) e
                end

            val (eds, (ts, maxName, ds, subs, by)) =
                ListUtil.foldlMapConcat
                (fn (ed, (ts, maxName, ds, subs, by)) =>
                    case #1 ed of
                        EDVal (p, t, e) =>
                        let
                            val e = doSubst (e, subs, by)

                            fun doVars ((p, _), ts) =
                                case p of
                                    PWild => ts
                                  | PVar xt => xt :: ts
                                  | PPrim _ => ts
                                  | PCon (_, _, _, NONE) => ts
                                  | PCon (_, _, _, SOME p) => doVars (p, ts)
                                  | PRecord xpcs =>
                                    foldl (fn ((_, p, _), ts) => doVars (p, ts))
                                          ts xpcs

                            fun bindOne subs = ((0, (ERel 0, #2 ed))
                                                :: map (fn (n, e) => (n + 1, E.liftExpInExp 0 e)) subs)

                            fun bindMany (n, subs) =
                                case n of
                                    0 => subs
                                  | _ => bindMany (n - 1, bindOne subs)
                        in
                            ([(EDVal (p, t, e), #2 ed)],
                             (doVars (p, ts),
                              maxName, ds,
                              bindMany (E.patBindsN p, subs),
                              by))
                        end
                      | EDValRec vis =>
                        let
                            val loc = #2 ed

                            val nr = length vis
                            val subsLocal = List.filter (fn (_, (ERel _, _)) => false
                                                          | _ => true) subs
                            val subsLocal = map (fn (n, e) => (n + nr, liftExpInExp nr 0 e))
                                                subsLocal

                            val vis = map (fn (x, t, e) =>
                                              (x, t, doSubst' (e, subsLocal))) vis

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

                            (*val () = Print.prefaces "Letto" [("e", ElabPrint.p_exp E.empty (old, ErrorMsg.dummySpan))]*)
                            (*val () = print ("A: " ^ Int.toString (length ts) ^ ", " ^ Int.toString (length ks) ^ "\n")*)
                            (*val () = app (fn (x, t) =>
                                             Print.prefaces "Var" [("x", Print.PD.string x),
                                                                   ("t", ElabPrint.p_con E.empty t)]) ts
                            val () = IS.app (fn n => print ("Free: " ^ Int.toString n ^ "\n")) efv*)

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

                            val subs = map (fn (n, e) => (n + nr,
                                                          case e of
                                                              (ERel _, _) => e
                                                            | _ => liftExpInExp nr 0 e))
                                           subs

                            val subs' = ListUtil.mapi (fn (i, (_, n, _, _)) =>
                                                          let
                                                              val e = (ENamed n, loc)

                                                              val e = IS.foldr (fn (x, e) =>
                                                                                   (ECApp (e, (CRel x, loc)), loc))
                                                                               e cfv

                                                              val e = IS.foldr (fn (x, e) =>
                                                                                   (EApp (e, (ERel (nr + x), loc)),
                                                                                    loc))
                                                                               e efv
                                                          in
                                                              (nr - i - 1, e)
                                                          end)
                                                      vis

                            val cfv = IS.listItems cfv
                            val efv = IS.listItems efv

                            val subs = subs' @ subs

                            val vis = map (fn (x, n, t, e) =>
                                              let
                                                  (*val () = Print.prefaces "preSubst"
                                                                          [("e", ElabPrint.p_exp E.empty e)]*)
                                                  val e = doSubst' (e, subs')

                                                  (*val () = Print.prefaces "squishCon"
                                                                          [("t", ElabPrint.p_con E.empty t)]*)
                                                  val t = squishCon cfv t
                                                  (*val () = Print.prefaces "squishExp"
                                                                          [("e", ElabPrint.p_exp E.empty e)]*)
                                                  val e = squishExp (nr, cfv, efv) e

                                                  (*val () = print ("Avail: " ^ Int.toString (length ts) ^ "\n")*)
                                                  val (e, t) = foldl (fn (ex, (e, t)) =>
                                                                         let
                                                                             (*val () = print (Int.toString ex ^ "\n")*)
                                                                             val (name, t') = List.nth (ts, ex)
                                                                             val t' = squishCon cfv t'
                                                                         in
                                                                             ((EAbs (name,
                                                                                     t',
                                                                                     t,
                                                                                     e), loc),
                                                                              (TFun (t',
                                                                                     t), loc))
                                                                         end)
                                                                     (e, t) efv
                                                  (*val () = print "Done\n"*)

                                                  val (e, t) = foldl (fn (cx, (e, t)) =>
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
                                                  (*Print.prefaces "Have a vi"
                                                                 [("x", Print.PD.string x),
                                                                  ("e", ElabPrint.p_exp ElabEnv.empty e)];*)
                                                  ("$" ^ x, n, t, e)
                                              end)
                                          vis

                            val ts = List.revAppend (map (fn (x, _, t, _) => (x, t)) vis, ts)
                        in
                            ([], (ts, maxName, vis @ ds, subs, by + nr))
                        end)
                (ts, #maxName st, #decls st, [], 0) eds

            val e' = doSubst (e, subs, by)
        in
            (*Print.prefaces "Before" [("e", ElabPrint.p_exp ElabEnv.empty e),
                                     ("se", ElabPrint.p_exp ElabEnv.empty (doSubst' (e, subs))),
                                     ("e'", ElabPrint.p_exp ElabEnv.empty e')];*)
            (*Print.prefaces "Let" [("Before", ElabPrint.p_exp ElabEnv.empty (old, ErrorMsg.dummySpan)),
                                  ("After", ElabPrint.p_exp ElabEnv.empty (ELet (eds, e', t), ErrorMsg.dummySpan))];*)
            (ELet (eds, e', t),
             {maxName = maxName,
              decls = ds})
            (*(ELet (eds, doSubst (liftExpInExp (~(length subs - numRemaining)) (length subs) e) subs),*)
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

                        val ds =
                            case #1 d of
                                DValRec vis => [(DValRec (vis @ #decls st), #2 d)]
                              | _ => [(DValRec (#decls st), #2 d), d]
                    in
                        (ds,
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
                  | DView _ => default ()
                  | DClass _ => default ()
                  | DDatabase _ => default ()
                  | DCookie _ => default ()
                  | DStyle _ => default ()
                  | DTask _ => explore ()
                  | DPolicy _ => explore ()
                  | DOnError _ => default ()
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
