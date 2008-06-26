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

structure Cloconv :> CLOCONV = struct

structure L = Mono
structure L' = Flat

structure IS = IntBinarySet

structure U = FlatUtil
structure E = FlatEnv

open Print.PD
open Print

val liftExpInExp =
    U.Exp.mapB {typ = fn t => t,
                exp = fn bound => fn e =>
                                     case e of
                                         L'.ERel xn =>
                                         if xn < bound then
                                             e
                                         else
                                             L'.ERel (xn + 1)
                                       | _ => e,
                bind = fn (bound, U.Exp.RelE _) => bound + 1
                        | (bound, _) => bound}
val subExpInExp =
    U.Exp.mapB {typ = fn t => t,
                exp = fn (xn, rep) => fn e =>
                                  case e of
                                      L'.ERel xn' =>
                                      (case Int.compare (xn', xn) of
                                           EQUAL => #1 rep
                                         | GREATER => L'.ERel (xn' - 1)
                                         | _ => e)
                                    | _ => e,
                bind = fn ((xn, rep), U.Exp.RelE _) => (xn+1, liftExpInExp 0 rep)
                        | (ctx, _) => ctx}


fun ccTyp (t, loc) =
    case t of
        L.TFun (t1, t2) => (L'.TFun (ccTyp t1, ccTyp t2), loc)
      | L.TRecord xts => (L'.TRecord (map (fn (x, t) => (x, ccTyp t)) xts), loc)
      | L.TNamed n => (L'.TNamed n, loc)
      | L.TFfi mx => (L'.TFfi mx, loc)

structure Ds :> sig
    type t

    val empty : t

    val exp : t -> string * int * L'.typ * L'.exp -> t
    val func : t -> string * L'.typ * L'.typ * L'.exp -> t * int
    val decls : t -> L'.decl list

    val enter : t -> t
    val used : t * int -> t
    val leave : t -> t
    val listUsed : t -> int list
end = struct

type t = int * L'.decl list * IS.set

val empty = (0, [], IS.empty)

fun exp (fc, ds, vm) (v as (_, _, _, (_, loc))) = (fc, (L'.DVal v, loc) :: ds, vm)

fun func (fc, ds, vm) (x, dom, ran, e as (_, loc)) =
    ((fc+1, (L'.DFun (fc, x, dom, ran, e), loc) :: ds, vm), fc)

fun decls (_, ds, _) = rev ds

fun enter (fc, ds, vm) = (fc, ds, IS.map (fn n => n + 1) vm)
fun used ((fc, ds, vm), n) = (fc, ds, IS.add (vm, n))
fun leave (fc, ds, vm) = (fc, ds, IS.map (fn n => n - 1) (IS.delete (vm, 0) handle NotFound => vm))

fun listUsed (_, _, vm) = IS.listItems vm

end


fun ccExp env ((e, loc), D) =
    case e of
        L.EPrim p => ((L'.EPrim p, loc), D)
      | L.ERel n => ((L'.ERel n, loc), Ds.used (D, n))
      | L.ENamed n => ((L'.ENamed n, loc), D)
      | L.EFfi mx => ((L'.EFfi mx, loc), D)
      | L.EFfiApp (m, x, es) =>
        let
            val (es, D) = ListUtil.foldlMap (ccExp env) D es
        in
            ((L'.EFfiApp (m, x, es), loc), D)
        end
      | L.EApp (e1, e2) =>
        let
            val (e1, D) = ccExp env (e1, D)
            val (e2, D) = ccExp env (e2, D)
        in
            ((L'.ELet ([("closure", (L'.TTop, loc), e1),
                        ("arg", (L'.TTop, loc), liftExpInExp 0 e2),
                        ("code", (L'.TTop, loc), (L'.EField ((L'.ERel 1, loc), "func"), loc)),
                        ("env", (L'.TTop, loc), (L'.EField ((L'.ERel 2, loc), "env"), loc))],
                       (L'.EApp ((L'.ERel 1, loc),
                                 (L'.ERecord [("env", (L'.ERel 0, loc), (L'.TTop, loc)),
                                              ("arg", (L'.ERel 2, loc), (L'.TTop, loc))], loc)), loc)), loc), D)
        end
      | L.EAbs (x, dom, ran, e) =>
        let
            val dom = ccTyp dom
            val ran = ccTyp ran
            val (e, D) = ccExp (E.pushERel env x dom) (e, Ds.enter D)
            val ns = Ds.listUsed D
            val ns = List.filter (fn n => n <> 0) ns
            val D = Ds.leave D

            (*val () = Print.preface ("Before", FlatPrint.p_exp FlatEnv.basis e)
            val () = List.app (fn (x, t) => preface ("Bound", box [string x,
                                                                   space,
                                                                   string ":",
                                                                   space,
                                                                   FlatPrint.p_typ env t]))
                     (E.listERels env)
            val () = List.app (fn n => preface ("Free", FlatPrint.p_exp (E.pushERel env x dom)
                                                        (L'.ERel n, loc))) ns*)
            val body = foldl (fn (n, e) =>
                                 subExpInExp (n, (L'.EField ((L'.ERel 1, loc), "fv" ^ Int.toString n), loc)) e)
                             e ns
            (*val () = Print.preface (" After", FlatPrint.p_exp FlatEnv.basis body)*)
            val body = (L'.ELet ([("env", (L'.TTop, loc), (L'.EField ((L'.ERel 0, loc), "env"), loc)),
                                  ("arg", (L'.TTop, loc), (L'.EField ((L'.ERel 1, loc), "arg"), loc))],
                                 body), loc)
                              
            val envT = (L'.TRecord (map (fn n => ("fv" ^ Int.toString n, #2 (E.lookupERel env (n-1)))) ns), loc)
            val (D, fi) = Ds.func D (x, (L'.TRecord [("env", envT), ("arg", dom)], loc), ran, body)
        in
            ((L'.ERecord [("code", (L'.ECode fi, loc), (L'.TTop, loc)),
                          ("env", (L'.ERecord (map (fn n => ("fv" ^ Int.toString n,
                                                             (L'.ERel (n-1), loc),
                                                             #2 (E.lookupERel env (n-1)))) ns), loc),
                           envT)], loc), D)
        end

      | L.ERecord xes =>
        let
            val (xes, D) = ListUtil.foldlMap (fn ((x, e, t), D) =>
                                                 let
                                                     val (e, D) = ccExp env (e, D)
                                                 in
                                                     ((x, e, ccTyp t), D)
                                                 end) D xes
        in
            ((L'.ERecord xes, loc), D)
        end
      | L.EField (e1, x) =>
        let
            val (e1, D) = ccExp env (e1, D)
        in
            ((L'.EField (e1, x), loc), D)
        end

fun ccDecl ((d, loc), D) =
    case d of
        L.DVal (x, n, t, e) =>
        let
            val t = ccTyp t
            val (e, D) = ccExp E.empty (e, D)
        in
            Ds.exp D (x, n, t, e)
        end

fun cloconv ds =
    let
        val D = foldl ccDecl Ds.empty ds
    in
        Ds.decls D
    end

end
