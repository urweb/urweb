(* Copyright (c) 2008-2009, Adam Chlipala
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

structure ElabEnv :> ELAB_ENV = struct

open Elab

structure U = ElabUtil

structure IM = IntBinaryMap
structure SM = BinaryMapFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

exception UnboundRel of int
exception UnboundNamed of int


(* AST utility functions *)

val liftKindInKind =
    U.Kind.mapB {kind = fn bound => fn k =>
                                       case k of
                                         KRel xn =>
                                         if xn < bound then
                                             k
                                         else
                                             KRel (xn + 1)
                                       | _ => k,
                 bind = fn (bound, _) => bound + 1}

val liftKindInCon =
    U.Con.mapB {kind = fn bound => fn k =>
                                      case k of
                                          KRel xn =>
                                          if xn < bound then
                                              k
                                          else
                                              KRel (xn + 1)
                                        | _ => k,
                con = fn _ => fn c => c,
                bind = fn (bound, U.Con.RelK _) => bound + 1
                        | (bound, _) => bound}

val liftConInCon =
    U.Con.mapB {kind = fn _ => fn k => k,
                con = fn bound => fn c =>
                                     case c of
                                         CRel xn =>
                                         if xn < bound then
                                             c
                                         else
                                             CRel (xn + 1)
                                       | CUnif (nl, loc, k, s, r) => CUnif (nl+1, loc, k, s, r)
                                       | _ => c,
                bind = fn (bound, U.Con.RelC _) => bound + 1
                        | (bound, _) => bound}

val lift = liftConInCon 0

fun mliftConInCon by c =
    if by = 0 then
        c
    else
        U.Con.mapB {kind = fn _ => fn k => k,
                    con = fn bound => fn c =>
                                         case c of
                                             CRel xn =>
                                             if xn < bound then
                                                 c
                                             else
                                                 CRel (xn + by)
                                           | CUnif (nl, loc, k, s, r) => CUnif (nl+by, loc, k, s, r)
                                           | _ => c,
                    bind = fn (bound, U.Con.RelC _) => bound + 1
                            | (bound, _) => bound} 0 c

val () = U.mliftConInCon := mliftConInCon

val liftKindInExp =
    U.Exp.mapB {kind = fn bound => fn k =>
                                      case k of
                                          KRel xn =>
                                          if xn < bound then
                                              k
                                          else
                                              KRel (xn + 1)
                                        | _ => k,
                con = fn _ => fn c => c,
                exp = fn _ => fn e => e,
                bind = fn (bound, U.Exp.RelK _) => bound + 1
                        | (bound, _) => bound}

val liftConInExp =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn bound => fn c =>
                                     case c of
                                         CRel xn =>
                                         if xn < bound then
                                             c
                                         else
                                             CRel (xn + 1)
                                       | CUnif (nl, loc, k, s, r) => CUnif (nl+1, loc, k, s, r)
                                       | _ => c,
                exp = fn _ => fn e => e,
                bind = fn (bound, U.Exp.RelC _) => bound + 1
                        | (bound, _) => bound}

val liftExpInExp =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn _ => fn c => c,
                exp = fn bound => fn e =>
                                     case e of
                                         ERel xn =>
                                         if xn < bound then
                                             e
                                         else
                                             ERel (xn + 1)
                                       | _ => e,
                bind = fn (bound, U.Exp.RelE _) => bound + 1
                        | (bound, _) => bound}


val liftExp = liftExpInExp 0

val subExpInExp =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn _ => fn c => c,
                exp = fn (xn, rep) => fn e =>
                                  case e of
                                      ERel xn' =>
                                      (case Int.compare (xn', xn) of
                                           EQUAL => #1 rep
                                         | GREATER=> ERel (xn' - 1)
                                         | LESS => e)
                                    | _ => e,
                bind = fn ((xn, rep), U.Exp.RelE _) => (xn+1, liftExpInExp 0 rep)
                        | ((xn, rep), U.Exp.RelC _) => (xn, liftConInExp 0 rep)
                        | (ctx, _) => ctx}

(* Back to environments *)

datatype 'a var' =
         Rel' of int * 'a
       | Named' of int * 'a

datatype 'a var =
         NotBound
       | Rel of int * 'a
       | Named of int * 'a

type datatyp = string list * (string * con option) IM.map

datatype class_name =
         ClNamed of int
       | ClProj of int * string list * string

fun class_name_out cn =
    case cn of
        ClNamed n => (CNamed n, ErrorMsg.dummySpan)
      | ClProj x => (CModProj x, ErrorMsg.dummySpan)

fun cn2s cn =
    case cn of
        ClNamed n => "Named(" ^ Int.toString n ^ ")"
      | ClProj (m, ms, x) => "Proj(" ^ Int.toString m ^ "," ^ String.concatWith "," ms ^ "," ^ x ^ ")"

structure CK = struct
type ord_key = class_name
open Order
fun compare x =
    case x of
        (ClNamed n1, ClNamed n2) => Int.compare (n1, n2)
      | (ClNamed _, _) => LESS
      | (_, ClNamed _) => GREATER

      | (ClProj (m1, ms1, x1), ClProj (m2, ms2, x2)) =>
        join (Int.compare (m1, m2),
              fn () => join (joinL String.compare (ms1, ms2),
                             fn () => String.compare (x1, x2)))
end

structure CS = BinarySetFn(CK)
structure CM = BinaryMapFn(CK)

type class = {ground : (con * exp) list,
              rules : (int * con list * con * exp) list}
val empty_class = {ground = [],
                   rules = []}

type env = {
     renameK : int SM.map,
     relK : string list,

     renameC : kind var' SM.map,
     relC : (string * kind) list,
     namedC : (string * kind * con option) IM.map,

     datatypes : datatyp IM.map,
     constructors : (datatype_kind * int * string list * con option * int) SM.map,

     classes : class CM.map,

     renameE : con var' SM.map,
     relE : (string * con) list,
     namedE : (string * con) IM.map,

     renameSgn : (int * sgn) SM.map,
     sgn : (string * sgn) IM.map,

     renameStr : (int * sgn) SM.map,
     str : (string * sgn) IM.map
}

val namedCounter = ref 0

fun newNamed () =
    let
        val r = !namedCounter
    in
        namedCounter := r + 1;
        r
    end

val empty = {
    renameK = SM.empty,
    relK = [],

    renameC = SM.empty,
    relC = [],
    namedC = IM.empty,

    datatypes = IM.empty,
    constructors = SM.empty,

    classes = CM.empty,

    renameE = SM.empty,
    relE = [],
    namedE = IM.empty,

    renameSgn = SM.empty,
    sgn = IM.empty,

    renameStr = SM.empty,
    str = IM.empty
}

fun pushKRel (env : env) x =
    let
        val renameK = SM.map (fn n => n+1) (#renameK env)
    in
        {renameK = SM.insert (renameK, x, 0),
         relK = x :: #relK env,

         renameC = SM.map (fn Rel' (n, k) => Rel' (n, liftKindInKind 0 k)
                            | x => x) (#renameC env),
         relC = map (fn (x, k) => (x, liftKindInKind 0 k)) (#relC env),
         namedC = #namedC env,

         datatypes = #datatypes env,
         constructors = #constructors env,

         classes = CM.map (fn cl => {ground = map (fn (c, e) =>
                                                      (liftKindInCon 0 c,
                                                       e))
                                                  (#ground cl),
                                     rules = #rules cl})
                          (#classes env),

         renameE = SM.map (fn Rel' (n, c) => Rel' (n, liftKindInCon 0 c)
                            | Named' (n, c) => Named' (n, c)) (#renameE env),
         relE = map (fn (x, c) => (x, liftKindInCon 0 c)) (#relE env),
         namedE = #namedE env,

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env
        }
    end

fun lookupKRel (env : env) n =
    (List.nth (#relK env, n))
    handle Subscript => raise UnboundRel n

fun lookupK (env : env) x = SM.find (#renameK env, x)

fun pushCRel (env : env) x k =
    let
        val renameC = SM.map (fn Rel' (n, k) => Rel' (n+1, k)
                               | x => x) (#renameC env)
    in
        {renameK = #renameK env,
         relK = #relK env,

         renameC = SM.insert (renameC, x, Rel' (0, k)),
         relC = (x, k) :: #relC env,
         namedC = #namedC env,

         datatypes = #datatypes env,
         constructors = #constructors env,

         classes = CM.map (fn class =>
                              {ground = map (fn (c, e) =>
                                                (liftConInCon 0 c,
                                                 e))
                                            (#ground class),
                               rules = #rules class})
                          (#classes env),

         renameE = SM.map (fn Rel' (n, c) => Rel' (n, lift c)
                            | Named' (n, c) => Named' (n, c)) (#renameE env),
         relE = map (fn (x, c) => (x, lift c)) (#relE env),
         namedE = #namedE env,

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env
        }
    end

fun lookupCRel (env : env) n =
    (List.nth (#relC env, n))
    handle Subscript => raise UnboundRel n

fun pushCNamedAs (env : env) x n k co =
    {renameK = #renameK env,
     relK = #relK env,

     renameC = SM.insert (#renameC env, x, Named' (n, k)),
     relC = #relC env,
     namedC = IM.insert (#namedC env, n, (x, k, co)),

     datatypes = #datatypes env,
     constructors = #constructors env,

     classes = #classes env,

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = #renameSgn env,
     sgn = #sgn env,
     
     renameStr = #renameStr env,
     str = #str env}

fun pushCNamed env x k co =
    let
        val n = !namedCounter
    in
        namedCounter := n + 1;
        (pushCNamedAs env x n k co, n)
    end

fun lookupCNamed (env : env) n =
    case IM.find (#namedC env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupC (env : env) x =
    case SM.find (#renameC env, x) of
        NONE => NotBound
      | SOME (Rel' x) => Rel x
      | SOME (Named' x) => Named x

fun pushDatatype (env : env) n xs xncs =
    let
        val dk = U.classifyDatatype xncs
    in
        {renameK = #renameK env,
         relK = #relK env,

         renameC = #renameC env,
         relC = #relC env,
         namedC = #namedC env,

         datatypes = IM.insert (#datatypes env, n,
                                (xs, foldl (fn ((x, n, to), cons) =>
                                               IM.insert (cons, n, (x, to))) IM.empty xncs)),
         constructors = foldl (fn ((x, n', to), cmap) =>
                                  SM.insert (cmap, x, (dk, n', xs, to, n)))
                              (#constructors env) xncs,

         classes = #classes env,

         renameE = #renameE env,
         relE = #relE env,
         namedE = #namedE env,

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env}
    end

fun lookupDatatype (env : env) n =
    case IM.find (#datatypes env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupDatatypeConstructor (_, dt) n =
    case IM.find (dt, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupConstructor (env : env) s = SM.find (#constructors env, s)

fun datatypeArgs (xs, _) = xs
fun constructors (_, dt) = IM.foldri (fn (n, (x, to), ls) => (x, n, to) :: ls) [] dt

fun listClasses (env : env) =
    map (fn (cn, {ground, rules}) =>
            (class_name_out cn,
             ground
             @ map (fn (nvs, cs, c, e) =>
                       let
                           val loc = #2 c
                           val c = foldr (fn (c', c) => (TFun (c', c), loc)) c cs
                           val c = ListUtil.foldli (fn (n, (), c) => (TCFun (Explicit,
                                                                             "x" ^ Int.toString n,
                                                                             (KError, loc),
                                                                             c), loc))
                                                   c (List.tabulate (nvs, fn _ => ()))
                       in
                           (c, e)
                       end) rules)) (CM.listItemsi (#classes env))

fun pushClass (env : env) n =
    {renameK = #renameK env,
     relK = #relK env,

     renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     datatypes = #datatypes env,
     constructors = #constructors env,

     classes = CM.insert (#classes env, ClNamed n, empty_class),

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = #renameSgn env,
     sgn = #sgn env,

     renameStr = #renameStr env,
     str = #str env}

fun class_name_in (c, _) =
    case c of
        CNamed n => SOME (ClNamed n)
      | CModProj x => SOME (ClProj x)
      | CUnif (_, _, _, _, ref (SOME c)) => class_name_in c
      | _ => NONE

fun isClass (env : env) c =
    let
        fun find NONE = false
          | find (SOME c) = Option.isSome (CM.find (#classes env, c))
    in
        find (class_name_in c)
    end

fun class_head_in c =
    case #1 c of
        CApp (f, _) => class_head_in f
      | CUnif (_, _, _, _, ref (SOME c)) => class_head_in c
      | _ => class_name_in c

exception Unify

fun unifyKinds (k1, k2) =
    case (#1 k1, #1 k2) of
        (KType, KType) => ()
      | (KArrow (d1, r1), KArrow (d2, r2)) => (unifyKinds (d1, d2); unifyKinds (r1, r2))
      | (KName, KName) => ()
      | (KRecord k1, KRecord k2) => unifyKinds (k1, k2)
      | (KUnit, KUnit) => ()
      | (KTuple ks1, KTuple ks2) => (ListPair.appEq unifyKinds (ks1, ks2)
                                     handle ListPair.UnequalLengths => raise Unify)
      | (KUnif (_, _, ref (SOME k1)), _) => unifyKinds (k1, k2)
      | (_, KUnif (_, _, ref (SOME k2))) => unifyKinds (k1, k2)
      | (KRel n1, KRel n2) => if n1 = n2 then () else raise Unify
      | (KFun (_, k1), KFun (_, k2)) => unifyKinds (k1, k2)
      | _ => raise Unify

fun eqCons (c1, c2) =
    case (#1 c1, #1 c2) of
        (CUnif (nl, _, _, _, ref (SOME c1)), _) => eqCons (mliftConInCon nl c1, c2)
      | (_, CUnif (nl, _, _, _, ref (SOME c2))) => eqCons (c1, mliftConInCon nl c2)

      | (CRel n1, CRel n2) => if n1 = n2 then () else raise Unify

      | (TFun (d1, r1), TFun (d2, r2)) => (eqCons (d1, d2); eqCons (r1, r2))
      | (TCFun (_, _, k1, r1), TCFun (_, _, k2, r2)) => (unifyKinds (k1, k2); eqCons (r1, r2))
      | (TRecord c1, TRecord c2) => eqCons (c1, c2)
      | (TDisjoint (a1, b1, c1), TDisjoint (a2, b2, c2)) =>
        (eqCons (a1, a2); eqCons (b1, b2); eqCons (c1, c2))

      | (CNamed n1, CNamed n2) => if n1 = n2 then () else raise Unify
      | (CModProj (n1, ms1, x1), CModProj (n2, ms2, x2)) =>
        if n1 = n2 andalso ms1 = ms2 andalso x1 = x2 then () else raise Unify
      | (CApp (f1, x1), CApp (f2, x2)) => (eqCons (f1, f2); eqCons (x1, x2))
      | (CAbs (_, k1, b1), CAbs (_, k2, b2)) => (unifyKinds (k1, k2); eqCons (b1, b2))

      | (CKAbs (_, b1), CKAbs (_, b2)) => eqCons (b1, b2)
      | (CKApp (c1, k1), CKApp (c2, k2)) => (eqCons (c1, c2); unifyKinds (k1, k2))
      | (TKFun (_, c1), TKFun (_, c2)) => eqCons (c1, c2)

      | (CName s1, CName s2) => if s1 = s2 then () else raise Unify

      | (CRecord (k1, xcs1), CRecord (k2, xcs2)) =>
        (unifyKinds (k1, k2);
         if length xcs1 <> length xcs2 then
             raise Unify
         else
             List.app (fn (x1, c1) =>
                          if List.exists (fn (x2, c2) => (eqCons (x1, x2); eqCons (c1, c2); true) handle Unify => false) xcs2 then
                              ()
                          else
                              raise Unify) xcs1)
      | (CConcat (f1, x1), CConcat (f2, x2)) => (eqCons (f1, f2); eqCons (x1, x2))
      | (CMap (d1, r1), CMap (d2, r2)) => (unifyKinds (d1, d2); unifyKinds (r1, r2))

      | (CUnit, CUnit) => ()

      | (CTuple cs1, CTuple cs2) => (ListPair.appEq (eqCons) (cs1, cs2)
                                     handle ListPair.UnequalLengths => raise Unify)
      | (CProj (c1, n1), CProj (c2, n2)) => (eqCons (c1, c2);
                                             if n1 = n2 then () else raise Unify)

      | _ => raise Unify

fun unifyCons (hnorm : con -> con) rs =
    let
        fun unify d (c1, c2) =
            case (#1 (hnorm c1), #1 (hnorm c2)) of
                (CUnif (nl, _, _, _, ref (SOME c1)), _) => unify d (mliftConInCon nl c1, c2)
              | (_, CUnif (nl, _, _, _, ref (SOME c2))) => unify d (c1, mliftConInCon nl c2)

              | (CUnif _, _) => ()

              | (c1', CRel n2) =>
                if n2 < d then
                    case c1' of
                        CRel n1 => if n1 = n2 then () else raise Unify
                      | _ => raise Unify
                else if n2 - d >= length rs then
                    case c1' of
                        CRel n1 => if n1 = n2 - length rs then () else raise Unify
                      | _ => raise Unify
                else
                    let
                        val r = List.nth (rs, n2 - d)
                    in
                        case !r of
                            NONE => r := SOME c1
                          | SOME c2 => eqCons (c1, c2)
                    end

              | (TFun (d1, r1), TFun (d2, r2)) => (unify d (d1, d2); unify d (r1, r2))
              | (TCFun (_, _, k1, r1), TCFun (_, _, k2, r2)) => (unifyKinds (k1, k2); unify (d + 1) (r1, r2))
              | (TRecord c1, TRecord c2) => unify d (c1, c2)
              | (TDisjoint (a1, b1, c1), TDisjoint (a2, b2, c2)) =>
                (unify d (a1, a2); unify d (b1, b2); unify d (c1, c2))

              | (CNamed n1, CNamed n2) => if n1 = n2 then () else raise Unify
              | (CModProj (n1, ms1, x1), CModProj (n2, ms2, x2)) =>
                if n1 = n2 andalso ms1 = ms2 andalso x1 = x2 then () else raise Unify
              | (CApp (f1, x1), CApp (f2, x2)) => (unify d (f1, f2); unify d (x1, x2))
              | (CAbs (_, k1, b1), CAbs (_, k2, b2)) => (unifyKinds (k1, k2); unify (d + 1) (b1, b2))

              | (CKAbs (_, b1), CKAbs (_, b2)) => unify d (b1, b2)
              | (CKApp (c1, k1), CKApp (c2, k2)) => (unify d (c1, c2); unifyKinds (k1, k2))
              | (TKFun (_, c1), TKFun (_, c2)) => unify d (c1, c2)

              | (CName s1, CName s2) => if s1 = s2 then () else raise Unify

              | (CRecord (k1, xcs1), CRecord (k2, xcs2)) =>
                (unifyKinds (k1, k2);
                 if length xcs1 <> length xcs2 then
                     raise Unify
                 else
                     app (fn (x1, c1) =>
                             if List.exists (fn (x2, c2) => (unify d (x1, x2); unify d (c1, c2); true) handle Unify => false) xcs2 then
                                 ()
                             else
                                 raise Unify) xcs1)
              | (CConcat (f1, x1), CConcat (f2, x2)) => (unify d (f1, f2); unify d (x1, x2))
              | (CMap (d1, r1), CMap (d2, r2)) => (unifyKinds (d1, d2); unifyKinds (r1, r2))

              | (CUnit, CUnit) => ()

              | (CTuple cs1, CTuple cs2) => (ListPair.appEq (unify d) (cs1, cs2)
                                             handle ListPair.UnequalLengths => raise Unify)
              | (CProj (c1, n1), CProj (c2, n2)) => (unify d (c1, c2);
                                                     if n1 = n2 then () else raise Unify)

              | _ => raise Unify
    in
        unify
    end

fun tryUnify hnorm nRs (c1, c2) =
    let
        val rs = List.tabulate (nRs, fn _ => ref NONE)
    in
        (unifyCons hnorm rs 0 (c1, c2);
         SOME (map (fn r => case !r of
                                NONE => raise Unify
                              | SOME c => c) rs))
        handle Unify => NONE
    end

fun unifySubst (rs : con list) =
    U.Con.mapB {kind = fn _ => fn k => k,
                con = fn d => fn c =>
                                 case c of
                                     CRel n =>
                                     if n < d then
                                         c
                                     else
                                         #1 (List.nth (rs, n - d))
                                   | _ => c,
                bind = fn (d, U.Con.RelC _) => d + 1
                        | (d, _) => d}
               0

exception Bad of con * con

val hasUnif = U.Con.exists {kind = fn _ => false,
                            con = fn CUnif (_, _, _, _, ref NONE) => true
                                   | _ => false}

fun startsWithUnif c =
    let
        fun firstArg (c, acc) =
            case #1 c of
                CApp (f, x) => firstArg (f, SOME x)
              | _ => acc
    in
        case firstArg (c, NONE) of
            NONE => false
          | SOME x => hasUnif x
    end

fun resolveClass (hnorm : con -> con) (consEq : con * con -> bool) (env : env) =
    let
        fun resolve c =
            let
                fun doHead f =
                    case CM.find (#classes env, f) of
                        NONE => NONE
                      | SOME class =>
                        let
                            val loc = #2 c

                            fun generalize (c as (_, loc)) =
                                case #1 c of
                                    CApp (f, x) =>
                                    let
                                        val (f, equate) = generalize f

                                        fun isRecord () =
                                            let
                                                val rk = ref NONE
                                                val k = (KUnif (loc, "k", rk), loc)
                                                val r = ref NONE
                                                val rc = (CUnif (0, loc, k, "x", r), loc)
                                            in
                                                ((CApp (f, rc), loc),
                                              fn () => (if consEq (rc, x) then
                                                            true
                                                        else
                                                            (raise Bad (rc, x);
                                                             false))
                                                       andalso equate ())
                                            end
                                    in
                                        case #1 x of
                                            CConcat _ => isRecord ()
                                          | CRecord _ => isRecord ()
                                          | _ => ((CApp (f, x), loc), equate)
                                    end
                                  | _ => (c, fn () => true)

                            val (c, equate) = generalize c

                            fun tryRules rules =
                                case rules of
                                    [] => NONE
                                  | (nRs, cs, c', e) :: rules' =>
                                    case tryUnify hnorm nRs (c, c') of
                                        NONE => tryRules rules'
                                      | SOME rs =>
                                        let
                                            val eos = map (resolve o unifySubst rs) cs
                                        in
                                            if List.exists (not o Option.isSome) eos
                                               orelse not (equate ())
                                               orelse not (consEq (c, unifySubst rs c')) then
                                                tryRules rules'
                                            else
                                                let
                                                    val es = List.mapPartial (fn x => x) eos

                                                    val e = foldr (fn (c, e) => (ECApp (e, c), loc)) e rs
                                                    val e = foldl (fn (e', e) => (EApp (e, e'), loc)) e es
                                                in
                                                    SOME e
                                                end
                                        end

                            fun rules () = tryRules (#rules class)
  
                            fun tryGrounds ces =
                                case ces of
                                    [] => rules ()
                                  | (c', e) :: ces' =>
                                    case tryUnify hnorm 0 (c, c') of
                                        NONE => tryGrounds ces'
                                      | SOME _ => SOME e
                        in
                            tryGrounds (#ground class)
                        end
            in
                if startsWithUnif c then
                    NONE
                else
                    case #1 c of
                        TRecord c =>
                        (case #1 (hnorm c) of
                             CRecord (_, xts) =>
                             let
                                 fun resolver (xts, acc) =
                                     case xts of
                                         [] => SOME (ERecord acc, #2 c)
                                       | (x, t) :: xts =>
                                         let
                                             val t = hnorm t

                                             val t = case t of
                                                         (CApp (f, x), loc) => (CApp (hnorm f, hnorm x), loc)
                                                       | _ => t
                                         in
                                             case resolve t of
                                                 NONE => NONE
                                               | SOME e => resolver (xts, (x, e, t) :: acc)
                                         end
                             in
                                 resolver (xts, [])
                             end
                           | _ => NONE)
                      | _ =>
                        case class_head_in c of
                            SOME f => doHead f
                          | _ => NONE
            end
    in
        resolve
    end

fun pushERel (env : env) x t =
    let
        val renameE = SM.map (fn Rel' (n, t) => Rel' (n+1, t)
                               | x => x) (#renameE env)

        val classes = CM.map (fn class =>
                                 {ground = map (fn (c, e) => (c, liftExp e)) (#ground class),
                                  rules = #rules class}) (#classes env)
        val classes = case class_head_in t of
                          NONE => classes
                        | SOME f =>
                          case CM.find (classes, f) of
                              NONE => classes
                            | SOME class =>
                              let
                                  val class = {ground = (t, (ERel 0, #2 t)) :: #ground class,
                                               rules = #rules class}
                              in
                                  CM.insert (classes, f, class)
                              end
    in
        {renameK = #renameK env,
         relK = #relK env,

         renameC = #renameC env,
         relC = #relC env,
         namedC = #namedC env,

         datatypes = #datatypes env,
         constructors = #constructors env,

         classes = classes,

         renameE = SM.insert (renameE, x, Rel' (0, t)),
         relE = (x, t) :: #relE env,
         namedE = #namedE env,

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env}
    end

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun rule_in c =
    let
        fun quantifiers (c, nvars) =
            case #1 c of
                TCFun (_, _, _, c) => quantifiers (c, nvars + 1)
              | _ =>
                let
                    fun clauses (c, hyps) =
                        case #1 c of
                            TFun (hyp, c) =>
                            (case class_head_in hyp of
                                 SOME _ => clauses (c, hyp :: hyps)
                               | NONE => NONE)
                          | _ =>
                            case class_head_in c of
                                NONE => NONE
                              | SOME f => SOME (f, nvars, rev hyps, c)
                in
                    clauses (c, [])
                end
    in
        quantifiers (c, 0)
    end

fun pushENamedAs (env : env) x n t =
    let
        val classes = #classes env
        val classes = case rule_in t of
                          NONE => classes
                        | SOME (f, nvs, cs, c) =>
                          case CM.find (classes, f) of
                              NONE => classes
                            | SOME class =>
                              let
                                  val e = (ENamed n, #2 t)

                                  val class =
                                      {ground = #ground class,
                                       rules = (nvs, cs, c, e) :: #rules class}
                              in
                                  CM.insert (classes, f, class)
                              end
    in
        {renameK = #renameK env,
         relK = #relK env,

         renameC = #renameC env,
         relC = #relC env,
         namedC = #namedC env,

         datatypes = #datatypes env,
         constructors = #constructors env,

         classes = classes,

         renameE = SM.insert (#renameE env, x, Named' (n, t)),
         relE = #relE env,
         namedE = IM.insert (#namedE env, n, (x, t)),

         renameSgn = #renameSgn env,
         sgn = #sgn env,
         
         renameStr = #renameStr env,
         str = #str env}
    end

fun pushENamed env x t =
    let
        val n = !namedCounter
    in
        namedCounter := n + 1;
        (pushENamedAs env x n t, n)
    end

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun checkENamed (env : env) n =
    Option.isSome (IM.find (#namedE env, n))

fun lookupE (env : env) x =
    case SM.find (#renameE env, x) of
        NONE => NotBound
      | SOME (Rel' x) => Rel x
      | SOME (Named' x) => Named x

fun pushSgnNamedAs (env : env) x n sgis =
    {renameK = #renameK env,
     relK = #relK env,

     renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     datatypes = #datatypes env,
     constructors = #constructors env,

     classes = #classes env,

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = SM.insert (#renameSgn env, x, (n, sgis)),
     sgn = IM.insert (#sgn env, n, (x, sgis)),
     
     renameStr = #renameStr env,
     str = #str env}

fun pushSgnNamed env x sgis =
    let
        val n = !namedCounter
    in
        namedCounter := n + 1;
        (pushSgnNamedAs env x n sgis, n)
    end

fun lookupSgnNamed (env : env) n =
    case IM.find (#sgn env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupSgn (env : env) x = SM.find (#renameSgn env, x)

fun lookupStrNamed (env : env) n =
    case IM.find (#str env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupStr (env : env) x = SM.find (#renameStr env, x)


fun sgiSeek (sgi, (sgns, strs, cons)) =
    case sgi of
        SgiConAbs (x, n, _) => (sgns, strs, IM.insert (cons, n, x))
      | SgiCon (x, n, _, _) => (sgns, strs, IM.insert (cons, n, x))
      | SgiDatatype dts => (sgns, strs, foldl (fn ((x, n, _, _), cons) => IM.insert (cons, n, x)) cons dts)
      | SgiDatatypeImp (x, n, _, _, _, _, _) => (sgns, strs, IM.insert (cons, n, x))
      | SgiVal _ => (sgns, strs, cons)
      | SgiSgn (x, n, _) => (IM.insert (sgns, n, x), strs, cons)
      | SgiStr (x, n, _) => (sgns, IM.insert (strs, n, x), cons)
      | SgiConstraint _ => (sgns, strs, cons)
      | SgiClassAbs (x, n, _) => (sgns, strs, IM.insert (cons, n, x))
      | SgiClass (x, n, _, _) => (sgns, strs, IM.insert (cons, n, x))

fun sgnSeek f sgis =
    let
        fun seek (sgis, sgns, strs, cons) =
            case sgis of
                [] => NONE
              | (sgi, _) :: sgis =>
                case f sgi of
                    SOME v =>
                    let
                        val cons =
                            case sgi of
                                SgiDatatype dts => foldl (fn ((x, n, _, _), cons) => IM.insert (cons, n, x)) cons dts
                              | SgiDatatypeImp (x, n, _, _, _, _, _) => IM.insert (cons, n, x)
                              | _ => cons
                    in
                        SOME (v, (sgns, strs, cons))
                    end
                  | NONE =>
                    let
                        val (sgns, strs, cons) = sgiSeek (sgi, (sgns, strs, cons))
                    in
                        seek (sgis, sgns, strs, cons)
                    end
    in
        seek (sgis, IM.empty, IM.empty, IM.empty)
    end

fun id x = x

fun unravelStr (str, _) =
    case str of
        StrVar x => (x, [])
      | StrProj (str, m) =>
        let
            val (x, ms) = unravelStr str
        in
            (x, ms @ [m])
        end
      | _ => raise Fail "unravelStr"

fun sgnS_con (str, (sgns, strs, cons)) c =
    case c of
        CModProj (m1, ms, x) =>
        (case IM.find (strs, m1) of
             NONE => c
           | SOME m1x =>
             let
                 val (m1, ms') = unravelStr str
             in
                 CModProj (m1, ms' @ m1x :: ms, x)
             end)
      | CNamed n =>
        (case IM.find (cons, n) of
             NONE => c
           | SOME nx =>
             let
                 val (m1, ms) = unravelStr str
             in
                 CModProj (m1, ms, nx)
             end)
      | _ => c

fun sgnS_con' (m1, ms', (sgns, strs, cons)) =
    U.Con.map {kind = fn x => x,
               con = fn c =>
                        case c of
                            CModProj (m1', ms, x) =>
                            (case IM.find (strs, m1') of
                                 NONE => c
                               | SOME m1x => CModProj (m1, ms' @ m1x :: ms, x))
                          | CNamed n =>
                            (case IM.find (cons, n) of
                                 NONE => c
                               | SOME nx => CModProj (m1, ms', nx))
                          | _ => c}

fun sgnS_sgn (str, (sgns, strs, cons)) sgn =
    case sgn of
        SgnProj (m1, ms, x) =>
        (case IM.find (strs, m1) of
             NONE => sgn
           | SOME m1x =>
             let
                 val (m1, ms') = unravelStr str
             in
                 SgnProj (m1, ms' @ m1x :: ms, x)
             end)
      | SgnVar n =>
        (case IM.find (sgns, n) of
             NONE => sgn
           | SOME nx =>
             let
                 val (m1, ms) = unravelStr str
             in
                 SgnProj (m1, ms, nx)
             end)
      | _ => sgn

fun sgnSubSgn x =
    ElabUtil.Sgn.map {kind = id,
                      con = sgnS_con x,
                      sgn_item = id,
                      sgn = sgnS_sgn x}



and projectSgn env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        (case sgnSeek (fn SgiSgn (x, _, sgn) => if x = field then SOME sgn else NONE | _ => NONE) sgis of
             NONE => NONE
           | SOME (sgn, subs) => SOME (sgnSubSgn (str, subs) sgn))
      | SgnError => SOME (SgnError, ErrorMsg.dummySpan)
      | _ => NONE

and hnormSgn env (all as (sgn, loc)) =
    case sgn of
        SgnError => all
      | SgnVar n => hnormSgn env (#2 (lookupSgnNamed env n))
      | SgnConst _ => all
      | SgnFun _ => all
      | SgnProj (m, ms, x) =>
        let
            val (_, sgn) = lookupStrNamed env m
        in
            case projectSgn env {str = foldl (fn (m, str) => (StrProj (str, m), loc)) (StrVar m, loc) ms,
                                 sgn = sgn,
                                 field = x} of
                NONE => raise Fail "ElabEnv.hnormSgn: projectSgn failed"
              | SOME sgn => hnormSgn env sgn
        end
      | SgnWhere (sgn, x, c) =>
        case #1 (hnormSgn env sgn) of
            SgnError => (SgnError, loc)
          | SgnConst sgis =>
            let
                fun traverse (pre, post) =
                    case post of
                        [] => raise Fail "ElabEnv.hnormSgn: Can't reduce 'where' [1]"
                      | (sgi as (SgiConAbs (x', n, k), loc)) :: rest =>
                        if x = x' then
                            List.revAppend (pre, (SgiCon (x', n, k, c), loc) :: rest)
                        else
                            traverse (sgi :: pre, rest)
                      | sgi :: rest => traverse (sgi :: pre, rest)

                val sgis = traverse ([], sgis)
            in
                (SgnConst sgis, loc)
            end
          | _ => raise Fail "ElabEnv.hnormSgn: Can't reduce 'where' [2]"

fun manifest (m, ms, loc) =
    foldl (fn (m, str) => (StrProj (str, m), loc)) (StrVar m, loc) ms

fun enrichClasses env classes (m1, ms) sgn =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        let
            val (classes, _, _, _) =
                foldl (fn (sgi, (classes, newClasses, fmap, env)) =>
                          let
                              fun found (x, n) =
                                  (CM.insert (classes,
                                              ClProj (m1, ms, x),
                                              empty_class),
                                   IM.insert (newClasses, n, x),
                                   sgiSeek (#1 sgi, fmap),
                                   env)

                              fun default () = (classes, newClasses, sgiSeek (#1 sgi, fmap), env)
                          in
                              case #1 sgi of
                                  SgiStr (x, _, sgn) =>
                                  let
                                      val str = manifest (m1, ms, #2 sgi)
                                      val sgn' = sgnSubSgn (str, fmap) sgn
                                  in
                                      (enrichClasses env classes (m1, ms @ [x]) sgn',
                                       newClasses,
                                       sgiSeek (#1 sgi, fmap),
                                       env)
                                  end
                                | SgiSgn (x, n, sgn) =>
                                  (classes,
                                   newClasses,
                                   fmap,
                                   pushSgnNamedAs env x n sgn)

                                | SgiClassAbs (x, n, _) => found (x, n)
                                | SgiClass (x, n, _, _) => found (x, n)
                                | SgiVal (x, n, c) =>
                                  (case rule_in c of
                                       NONE => default ()
                                     | SOME (cn, nvs, cs, c) =>
                                       let
                                           val loc = #2 c
                                           val globalize = sgnS_con' (m1, ms, fmap)

                                           val nc =
                                               case cn of
                                                   ClNamed f => IM.find (newClasses, f)
                                                 | _ => NONE
                                       in
                                           case nc of
                                               NONE =>
                                               let
                                                   val classes =
                                                       case CM.find (classes, cn) of
                                                           NONE => classes
                                                         | SOME class =>
                                                           let
                                                               val e = (EModProj (m1, ms, x), #2 sgn)

                                                               val class =
                                                                   {ground = #ground class,
                                                                    rules = (nvs,
                                                                             map globalize cs,
                                                                             globalize c,
                                                                             e) :: #rules class}
                                                           in
                                                               CM.insert (classes, cn, class)
                                                           end
                                               in
                                                   (classes,
                                                    newClasses,
                                                    fmap,
                                                    env)
                                               end
                                             | SOME fx =>
                                               let
                                                   val cn = ClProj (m1, ms, fx)

                                                   val classes =
                                                       case CM.find (classes, cn) of
                                                           NONE => classes
                                                         | SOME class =>
                                                           let
                                                               val e = (EModProj (m1, ms, x), #2 sgn)

                                                               val class = 
                                                                   {ground = #ground class,
                                                                    rules = (nvs,
                                                                             map globalize cs,
                                                                             globalize c,
                                                                             e) :: #rules class}
                                                           in
                                                               CM.insert (classes, cn, class)
                                                           end
                                               in
                                                   (classes,
                                                    newClasses,
                                                    fmap,
                                                    env)
                                               end
                                       end)
                                | _ => default ()
                          end)
                      (classes, IM.empty, (IM.empty, IM.empty, IM.empty), env) sgis
        in
            classes
        end
      | _ => classes

fun pushStrNamedAs (env : env) x n sgn =
    {renameK = #renameK env,
     relK = #relK env,

     renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     datatypes = #datatypes env,
     constructors = #constructors env,

     classes = enrichClasses env (#classes env) (n, []) sgn,

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = #renameSgn env,
     sgn = #sgn env,

     renameStr = SM.insert (#renameStr env, x, (n, sgn)),
     str = IM.insert (#str env, n, (x, sgn))}

fun pushStrNamed env x sgn =
    let
        val n = !namedCounter
    in
        namedCounter := n + 1;
        (pushStrNamedAs env x n sgn, n)
    end

fun sgiBinds env (sgi, loc) =
    case sgi of
        SgiConAbs (x, n, k) => pushCNamedAs env x n k NONE
      | SgiCon (x, n, k, c) => pushCNamedAs env x n k (SOME c)
      | SgiDatatype dts =>
        let
            fun doOne ((x, n, xs, xncs), env) =
                let
                    val k = (KType, loc)
                    val k' = foldr (fn (_, k') => (KArrow (k, k'), loc)) k xs

                    val env = pushCNamedAs env x n k' NONE
                in
                    foldl (fn ((x', n', to), env) =>
                              let
                                  val t =
                                      case to of
                                          NONE => (CNamed n, loc)
                                        | SOME t => (TFun (t, (CNamed n, loc)), loc)

                                  val k = (KType, loc)
                                  val t = foldr (fn (x, t) => (TCFun (Explicit, x, k, t), loc)) t xs
                              in
                                  pushENamedAs env x' n' t
                              end)
                          env xncs
                end
        in
            foldl doOne env dts
        end
      | SgiDatatypeImp (x, n, m1, ms, x', xs, xncs) =>
        let
            val k = (KType, loc)
            val k' = foldr (fn (_, k') => (KArrow (k, k'), loc)) k xs

            val env = pushCNamedAs env x n k' (SOME (CModProj (m1, ms, x'), loc))
        in
            foldl (fn ((x', n', to), env) =>
                      let
                          val t =
                              case to of
                                  NONE => (CNamed n, loc)
                                | SOME t => (TFun (t, (CNamed n, loc)), loc)

                          val k = (KType, loc)
                          val t = foldr (fn (x, t) => (TCFun (Explicit, x, k, t), loc)) t xs
                      in
                          pushENamedAs env x' n' t
                      end)
            env xncs
        end
      | SgiVal (x, n, t) => pushENamedAs env x n t
      | SgiStr (x, n, sgn) => pushStrNamedAs env x n sgn
      | SgiSgn (x, n, sgn) => pushSgnNamedAs env x n sgn
      | SgiConstraint _ => env

      | SgiClassAbs (x, n, k) => pushCNamedAs env x n k NONE
      | SgiClass (x, n, k, c) => pushCNamedAs env x n k (SOME c)

fun sgnSubCon x =
    ElabUtil.Con.map {kind = id,
                      con = sgnS_con x}

fun projectStr env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        (case sgnSeek (fn SgiStr (x, _, sgn) => if x = field then SOME sgn else NONE | _ => NONE) sgis of
             NONE => NONE
           | SOME (sgn, subs) => SOME (sgnSubSgn (str, subs) sgn))
      | SgnError => SOME (SgnError, ErrorMsg.dummySpan)
      | _ => NONE

fun chaseMpath env (n, ms) =
    let
        val (_, sgn) = lookupStrNamed env n
    in
        foldl (fn (m, (str, sgn)) =>
                                   case projectStr env {sgn = sgn, str = str, field = m} of
                                       NONE => raise Fail "kindof: Unknown substructure"
                                     | SOME sgn => ((StrProj (str, m), #2 sgn), sgn))
                               ((StrVar n, #2 sgn), sgn) ms
    end

fun projectCon env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        (case sgnSeek (fn SgiConAbs (x, _, k) => if x = field then SOME (k, NONE) else NONE
                        | SgiCon (x, _, k, c) => if x = field then SOME (k, SOME c) else NONE
                        | SgiDatatype dts =>
                          (case List.find (fn (x, _, xs, _) => x = field) dts of
                               SOME (_, _, xs, _) =>
                               let
                                   val k = (KType, #2 sgn)
                                   val k' = foldl (fn (_, k') => (KArrow (k, k'), #2 sgn)) k xs
                               in
                                   SOME (k', NONE)
                                   end
                             | NONE => NONE)
                        | SgiDatatypeImp (x, _, m1, ms, x', xs, _) =>
                          if x = field then
                              let
                                  val k = (KType, #2 sgn)
                                  val k' = foldl (fn (_, k') => (KArrow (k, k'), #2 sgn)) k xs
                              in
                                  SOME (k', SOME (CModProj (m1, ms, x'), #2 sgn))
                              end
                          else
                              NONE
                        | SgiClassAbs (x, _, k) => if x = field then
                                                       SOME (k, NONE)
                                                   else
                                                       NONE
                        | SgiClass (x, _, k, c) => if x = field then
                                                       SOME (k, SOME c)
                                                   else
                                                       NONE
                        | _ => NONE) sgis of
             NONE => NONE
           | SOME ((k, co), subs) => SOME (k, Option.map (sgnSubCon (str, subs)) co))
      | SgnError => SOME ((KError, ErrorMsg.dummySpan), SOME (CError, ErrorMsg.dummySpan))
      | _ => NONE

fun projectDatatype env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        (case sgnSeek (fn SgiDatatype dts =>
                          (case List.find (fn (x, _, _, _) => x = field) dts of
                               SOME (_, _, xs, xncs) => SOME (xs, xncs)
                             | NONE => NONE)
                        | SgiDatatypeImp (x, _, _, _, _, xs, xncs) => if x = field then SOME (xs, xncs) else NONE
                        | _ => NONE) sgis of
             NONE => NONE
           | SOME ((xs, xncs), subs) => SOME (xs,
                                              map (fn (x, n, to) => (x, n, Option.map (sgnSubCon (str, subs)) to)) xncs))
      | _ => NONE

fun projectConstructor env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        let
            fun consider (n, xs, xncs) =
                ListUtil.search (fn (x, n', to) =>
                                    if x <> field then
                                        NONE
                                    else
                                        SOME (U.classifyDatatype xncs, n', xs, to, (CNamed n, #2 str))) xncs
        in
            case sgnSeek (fn SgiDatatype dts =>
                             let
                                 fun search dts =
                                     case dts of
                                         [] => NONE
                                       | (_, n, xs, xncs) :: dts =>
                                         case consider (n, xs, xncs) of
                                             NONE => search dts
                                           | v => v
                             in
                                 search dts
                             end
                           | SgiDatatypeImp (_, n, _, _, _, xs, xncs) => consider (n, xs, xncs)
                           | _ => NONE) sgis of
                NONE => NONE
              | SOME ((dk, n, xs, to, t), subs) => SOME (dk, n, xs, Option.map (sgnSubCon (str, subs)) to,
                                                         sgnSubCon (str, subs) t)
        end
      | _ => NONE

fun projectVal env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        let
            fun seek (n, xs, xncs) =
                ListUtil.search (fn (x, _, to) =>
                                    if x = field then
                                        SOME (let
                                                  val base = (CNamed n, #2 sgn)
                                                  val nxs = length xs
                                                  val base = ListUtil.foldli (fn (i, _, base) =>
                                                                                 (CApp (base,
                                                                                       (CRel (nxs - i - 1), #2 sgn)),
                                                                                  #2 sgn))
                                                                             base xs

                                                  val t =
                                                      case to of
                                                          NONE => base
                                                        | SOME t => (TFun (t, base), #2 sgn)
                                                  val k = (KType, #2 sgn)
                                              in
                                                  foldr (fn (x, t) => (TCFun (Implicit, x, k, t), #2 sgn))
                                                  t xs
                                              end)
                                    else
                                        NONE) xncs
        in
            case sgnSeek (fn SgiVal (x, _, c) => if x = field then SOME c else NONE
                           | SgiDatatype dts =>
                             let
                                 fun search dts =
                                     case dts of
                                         [] => NONE
                                       | (_, n, xs, xncs) :: dts =>
                                         case seek (n, xs, xncs) of
                                             NONE => search dts
                                           | v => v
                             in
                                 search dts
                             end
                           | SgiDatatypeImp (_, n, _, _, _, xs, xncs) => seek (n, xs, xncs)
                           | _ => NONE) sgis of
                NONE => NONE
              | SOME (c, subs) => SOME (sgnSubCon (str, subs) c)
        end
      | SgnError => SOME (CError, ErrorMsg.dummySpan)
      | _ => NONE

fun sgnSeekConstraints (str, sgis) =
    let
        fun seek (sgis, sgns, strs, cons, acc) =
            case sgis of
                [] => acc
              | (sgi, _) :: sgis =>
                case sgi of
                    SgiConstraint (c1, c2) =>
                    let
                        val sub = sgnSubCon (str, (sgns, strs, cons))
                    in
                        seek (sgis, sgns, strs, cons, (sub c1, sub c2) :: acc)
                    end
                  | SgiConAbs (x, n, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
                  | SgiCon (x, n, _, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
                  | SgiDatatype dts => seek (sgis, sgns, strs,
                                             foldl (fn ((x, n, _, _), cons) => IM.insert (cons, n, x)) cons dts, acc)
                  | SgiDatatypeImp (x, n, _, _, _, _, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
                  | SgiVal _ => seek (sgis, sgns, strs, cons, acc)
                  | SgiSgn (x, n, _) => seek (sgis, IM.insert (sgns, n, x), strs, cons, acc)
                  | SgiStr (x, n, _) => seek (sgis, sgns, IM.insert (strs, n, x), cons, acc)
                  | SgiClassAbs (x, n, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
                  | SgiClass (x, n, _, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
    in
        seek (sgis, IM.empty, IM.empty, IM.empty, [])
    end

fun projectConstraints env {sgn, str} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis => SOME (sgnSeekConstraints (str, sgis))
      | SgnError => SOME []
      | _ => NONE

fun patBinds env (p, loc) =
    case p of
        PWild => env
      | PVar (x, t) => pushERel env x t
      | PPrim _ => env
      | PCon (_, _, _, NONE) => env
      | PCon (_, _, _, SOME p) => patBinds env p
      | PRecord xps => foldl (fn ((_, p, _), env) => patBinds env p) env xps

fun patBindsN (p, _) =
    case p of
        PWild => 0
      | PVar _ => 1
      | PPrim _ => 0
      | PCon (_, _, _, NONE) => 0
      | PCon (_, _, _, SOME p) => patBindsN p
      | PRecord xps => foldl (fn ((_, p, _), n) => patBindsN p + n) 0 xps

fun edeclBinds env (d, loc) =
    case d of
        EDVal (p, _, _) => patBinds env p
      | EDValRec vis => foldl (fn ((x, t, _), env) => pushERel env x t) env vis

fun declBinds env (d, loc) =
    case d of
        DCon (x, n, k, c) => pushCNamedAs env x n k (SOME c)
      | DDatatype dts =>
        let
            fun doOne ((x, n, xs, xncs), env) =
                let
                    val k = (KType, loc) 
                    val nxs = length xs
                    val (tb, kb) = ListUtil.foldli (fn (i, x', (tb, kb)) =>
                                                       ((CApp (tb, (CRel (nxs - i - 1), loc)), loc),
                                                        (KArrow (k, kb), loc)))
                                                   ((CNamed n, loc), k) xs
                                   
                    val env = pushCNamedAs env x n kb NONE
                    val env = pushDatatype env n xs xncs
                in
                    foldl (fn ((x', n', to), env) =>
                              let
                                  val t =
                                      case to of
                                          NONE => tb
                                        | SOME t => (TFun (t, tb), loc)
                                  val t = foldr (fn (x, t) => (TCFun (Implicit, x, k, t), loc)) t xs
                              in
                                  pushENamedAs env x' n' t
                              end)
                          env xncs
                end
        in
            foldl doOne env dts
        end
      | DDatatypeImp (x, n, m, ms, x', xs, xncs) =>
        let
            val t = (CModProj (m, ms, x'), loc)
            val k = (KType, loc) 
            val nxs = length xs
            val (tb, kb) = ListUtil.foldli (fn (i, x', (tb, kb)) =>
                                               ((CApp (tb, (CRel (nxs - i - 1), loc)), loc),
                                                (KArrow (k, kb), loc)))
                                           ((CNamed n, loc), k) xs

            val env = pushCNamedAs env x n kb (SOME t)
            val env = pushDatatype env n xs xncs
        in
            foldl (fn ((x', n', to), env) =>
                      let
                          val t =
                              case to of
                                  NONE => tb
                                | SOME t => (TFun (t, tb), loc)
                          val t = foldr (fn (x, t) => (TCFun (Implicit, x, k, t), loc)) t xs
                      in
                          pushENamedAs env x' n' t
                      end)
                  env xncs
        end
      | DVal (x, n, t, _) => pushENamedAs env x n t
      | DValRec vis => foldl (fn ((x, n, t, _), env) => pushENamedAs env x n t) env vis
      | DSgn (x, n, sgn) => pushSgnNamedAs env x n sgn
      | DStr (x, n, sgn, _) => pushStrNamedAs env x n sgn
      | DFfiStr (x, n, sgn) => pushStrNamedAs env x n sgn
      | DConstraint _ => env
      | DExport _ => env
      | DTable (tn, x, n, c, _, pc, _, cc) =>
        let
            val ct = (CModProj (tn, [], "sql_table"), loc)
            val ct = (CApp (ct, c), loc)
            val ct = (CApp (ct, (CConcat (pc, cc), loc)), loc)
        in
            pushENamedAs env x n ct
        end
      | DSequence (tn, x, n) =>
        let
            val t = (CModProj (tn, [], "sql_sequence"), loc)
        in
            pushENamedAs env x n t
        end
      | DView (tn, x, n, _, c) =>
        let
            val ct = (CModProj (tn, [], "sql_view"), loc)
            val ct = (CApp (ct, c), loc)
        in
            pushENamedAs env x n ct
        end
      | DClass (x, n, k, c) =>
        let
            val k = (KArrow (k, (KType, loc)), loc)
            val env = pushCNamedAs env x n k (SOME c)
        in
            pushClass env n
        end
      | DDatabase _ => env
      | DCookie (tn, x, n, c) =>
        let
            val t = (CApp ((CModProj (tn, [], "cookie"), loc), c), loc)
        in
            pushENamedAs env x n t
        end
      | DStyle (tn, x, n) =>
        let
            val t = (CModProj (tn, [], "css_class"), loc)
        in
            pushENamedAs env x n t
        end
      | DTask _ => env
      | DPolicy _ => env
      | DOnError _ => env

end
