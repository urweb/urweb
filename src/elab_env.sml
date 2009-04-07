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

exception SynUnif

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
                                       (*| CUnif _ => raise SynUnif*)
                                       | _ => c,
                bind = fn (bound, U.Con.RelC _) => bound + 1
                        | (bound, _) => bound}

val lift = liftConInCon 0

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

datatype class_key =
         CkNamed of int
       | CkRel of int
       | CkProj of int * string list * string
       | CkApp of class_key * class_key
       | CkOther of con

fun ck2s ck =
    case ck of
        CkNamed n => "Named(" ^ Int.toString n ^ ")"
      | CkRel n => "Rel(" ^ Int.toString n ^ ")"
      | CkProj (m, ms, x) => "Proj(" ^ Int.toString m ^ "," ^ String.concatWith "," ms ^ "," ^ x ^ ")"
      | CkApp (ck1, ck2) => "App(" ^ ck2s ck1 ^ ", " ^ ck2s ck2 ^ ")"
      | CkOther _ => "Other"

type class_key_n = class_key * int

fun ckn2s (ck, n) = ck2s ck ^ "[" ^ Int.toString n ^ "]"

fun cp2s (cn, ck) = "(" ^ cn2s cn ^ "," ^ ck2s ck ^ ")"

structure KK = struct
type ord_key = class_key_n
open Order
fun compare' x =
    case x of
        (CkNamed n1, CkNamed n2) => Int.compare (n1, n2)
      | (CkNamed _, _) => LESS
      | (_, CkNamed _) => GREATER

      | (CkRel n1, CkRel n2) => Int.compare (n1, n2)
      | (CkRel _, _) => LESS
      | (_, CkRel _) => GREATER

      | (CkProj (m1, ms1, x1), CkProj (m2, ms2, x2)) =>
        join (Int.compare (m1, m2),
              fn () => join (joinL String.compare (ms1, ms2),
                             fn () => String.compare (x1, x2)))
      | (CkProj _, _) => LESS
      | (_, CkProj _) => GREATER

      | (CkApp (f1, x1), CkApp (f2, x2)) =>
        join (compare' (f1, f2),
              fn () => compare' (x1, x2))
      | (CkApp _, _) => LESS
      | (_, CkApp _) => GREATER

      | (CkOther _, CkOther _) => EQUAL
fun compare ((k1, n1), (k2, n2)) =
    join (Int.compare (n1, n2),
       fn () => compare' (k1, k2))
end

structure KM = BinaryMapFn(KK)

type class = {ground : ((class_name * class_key) list * exp) KM.map,
              inclusions : exp CM.map}
val empty_class = {ground = KM.empty,
                   inclusions = CM.empty}

fun printClasses cs = (print "Classes:\n";
                       CM.appi (fn (cn, {ground = km, ...} : class) =>
                                   (print (cn2s cn ^ ":");
                                    KM.appi (fn (ck, _) => print (" " ^ ckn2s ck)) km;
                                    print "\n")) cs)

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

fun liftClassKey' ck =
    case ck of
        CkNamed _ => ck
      | CkRel n => CkRel (n + 1)
      | CkProj _ => ck
      | CkApp (ck1, ck2) => CkApp (liftClassKey' ck1, liftClassKey' ck2)
      | CkOther c => CkOther (lift c)

fun liftClassKey (ck, n) = (liftClassKey' ck, n)

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

         classes = #classes env,

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
                              {ground = KM.foldli (fn (ck, e, km) =>
                                                      KM.insert (km, liftClassKey ck, e))
                                                  KM.empty (#ground class),
                               inclusions = #inclusions class})
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
      | CUnif (_, _, _, ref (SOME c)) => class_name_in c
      | _ => NONE

fun isClass (env : env) c =
    let
        fun find NONE = false
          | find (SOME c) = Option.isSome (CM.find (#classes env, c))
    in
        find (class_name_in c)
    end

fun class_key_in (all as (c, _)) =
    case c of
        CRel n => CkRel n
      | CNamed n => CkNamed n
      | CModProj x => CkProj x
      | CUnif (_, _, _, ref (SOME c)) => class_key_in c
      | CApp (c1, c2) => CkApp (class_key_in c1, class_key_in c2)
      | _ => CkOther all

fun class_key_out loc =
    let
        fun cko k =
            case k of
                CkRel n => (CRel n, loc)
              | CkNamed n => (CNamed n, loc)
              | CkProj x => (CModProj x, loc)
              | CkApp (k1, k2) => (CApp (cko k1, cko k2), loc)
              | CkOther c => c
    in
        cko
    end

fun class_pair_in (c, _) =
    case c of
        CApp (f, x) =>
        (case class_name_in f of
             SOME f => SOME (f, class_key_in x)
           | _ => NONE)
      | CUnif (_, _, _, ref (SOME c)) => class_pair_in c
      | _ => NONE

fun sub_class_key (n, c) =
    let
        fun csk k =
            case k of
                CkRel n' => SOME (if n' = n then
                                      c
                                  else
                                      k)
              | CkNamed _ => SOME k
              | CkProj _ => SOME k
              | CkApp (k1, k2) =>
                (case (csk k1, csk k2) of
                     (SOME k1, SOME k2) => SOME (CkApp (k1, k2))
                   | _ => NONE)
              | CkOther _ => NONE
    in
        csk
    end

fun resolveClass (env : env) c =
    let
        fun doPair (f, x) =
            case CM.find (#classes env, f) of
                NONE => NONE
              | SOME class =>
                let
                    val loc = #2 c

                    fun tryIncs () =
                        let
                            fun tryIncs fs =
                                case fs of
                                    [] => NONE
                                  | (f', e') :: fs =>
                                    case doPair (f', x) of
                                        NONE => tryIncs fs
                                      | SOME e =>
                                        let
                                            val e' = (ECApp (e', class_key_out loc x), loc)
                                            val e' = (EApp (e', e), loc)
                                        in
                                            SOME e'
                                        end
                        in
                            tryIncs (CM.listItemsi (#inclusions class))
                        end

                    fun tryRules (k, args) =
                        let
                            val len = length args

                            fun tryNext () =
                                case k of
                                    CkApp (k1, k2) => tryRules (k1, k2 :: args)
                                  | _ => tryIncs ()
                        in
                            case KM.find (#ground class, (k, length args)) of
                                SOME (cs, e) =>
                                let
                                    val es = map (fn (cn, ck) =>
                                                     let
                                                         val ck = ListUtil.foldli (fn (i, arg, ck) =>
                                                                                      case ck of
                                                                                          NONE => NONE
                                                                                        | SOME ck =>
                                                                                          sub_class_key (len - i - 1,
                                                                                                         arg)
                                                                                                        ck)
                                                                                  (SOME ck) args
                                                     in
                                                         case ck of
                                                             NONE => NONE
                                                           | SOME ck => doPair (cn, ck)
                                                     end) cs
                                in
                                    if List.exists (not o Option.isSome) es then
                                        tryNext ()
                                    else
                                        let
                                            val e = foldl (fn (arg, e) => (ECApp (e, class_key_out loc arg), loc))
                                                          e args
                                            val e = foldr (fn (pf, e) => (EApp (e, pf), loc))
                                                          e (List.mapPartial (fn x => x) es)
                                        in
                                            SOME e
                                        end
                                end
                              | NONE => tryNext ()
                        end
                in
                    tryRules (x, [])
                end
    in
        case class_pair_in c of
            SOME p => doPair p
          | _ => NONE
    end

fun pushERel (env : env) x t =
    let
        val renameE = SM.map (fn Rel' (n, t) => Rel' (n+1, t)
                               | x => x) (#renameE env)

        val classes = CM.map (fn class =>
                                 {ground = KM.map (fn (ps, e) => (ps, liftExp e)) (#ground class),
                                  inclusions = #inclusions class}) (#classes env)
        val classes = case class_pair_in t of
                          NONE => classes
                        | SOME (f, x) =>
                          case CM.find (classes, f) of
                              NONE => classes
                            | SOME class =>
                              let
                                  val class = {ground = KM.insert (#ground class, (x, 0), ([], (ERel 0, #2 t))),
                                               inclusions = #inclusions class}
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

datatype rule =
         Normal of int * (class_name * class_key) list * class_key
       | Inclusion of class_name

fun containsOther k =
    case k of
        CkOther _ => true
      | CkApp (k1, k2) => containsOther k1 orelse containsOther k2
      | _ => false

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
                            (case class_pair_in hyp of
                                 SOME (p as (_, CkRel _)) => clauses (c, p :: hyps)
                               | _ => NONE)
                          | _ =>
                            case class_pair_in c of
                                NONE => NONE
                              | SOME (cn, ck) =>
                                let
                                    fun dearg (ck, i) =
                                        if i >= nvars then
                                            if containsOther ck
                                               orelse List.exists (fn (_, k) => containsOther k) hyps then
                                                NONE
                                            else
                                                SOME (cn, Normal (nvars, hyps, ck))
                                        else case ck of
                                                 CkApp (ck, CkRel i') =>
                                                 if i' = i then
                                                     dearg (ck, i + 1)
                                                 else
                                                     NONE
                                               | _ => NONE
                                in
                                    dearg (ck, 0)
                                end
                in
                    clauses (c, [])
                end
    in
        case #1 c of
            TCFun (_, _, _, (TFun ((CApp (f1, (CRel 0, _)), _),
                                   (CApp (f2, (CRel 0, _)), _)), _)) =>
            (case (class_name_in f1, class_name_in f2) of
                 (SOME f1, SOME f2) => SOME (f2, Inclusion f1)
               | _ => NONE)
          | _ => quantifiers (c, 0)
    end

fun inclusion (classes : class CM.map, init, inclusions, f, e : exp) =
    let
        fun search (f, fs) =
            if f = init then
                NONE
            else if CS.member (fs, f) then
                SOME fs
            else
                let
                    val fs = CS.add (fs, f)
                in
                    case CM.find (classes, f) of
                        NONE => SOME fs
                      | SOME {inclusions = fs', ...} =>
                        CM.foldli (fn (f', _, fs) =>
                                      case fs of
                                          NONE => NONE
                                        | SOME fs => search (f', fs)) (SOME fs) fs'
                end
    in
        case search (f, CS.empty) of
            SOME _ => CM.insert (inclusions, f, e)
          | NONE => (ErrorMsg.errorAt (#2 e) "Type class inclusion would create a cycle";
                     inclusions)
    end

fun pushENamedAs (env : env) x n t =
    let
        val classes = #classes env
        val classes = case rule_in t of
                          NONE => classes
                        | SOME (f, rule) =>
                          case CM.find (classes, f) of
                              NONE => classes
                            | SOME class =>
                              let
                                  val e = (ENamed n, #2 t)

                                  val class =
                                      case rule of
                                          Normal (nvars, hyps, x) =>
                                          {ground = KM.insert (#ground class, (x, nvars), (hyps, e)),
                                           inclusions = #inclusions class}
                                        | Inclusion f' =>
                                          {ground = #ground class,
                                           inclusions = inclusion (classes, f, #inclusions class, f', e)}
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
      | SgiDatatype (x, n, _, _) => (sgns, strs, IM.insert (cons, n, x))
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
                                SgiDatatype (x, n, _, _) => IM.insert (cons, n, x)
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

fun sgnS_con' (arg as (m1, ms', (sgns, strs, cons))) c =
    case c of
        CModProj (m1, ms, x) =>
        (case IM.find (strs, m1) of
             NONE => c
           | SOME m1x => CModProj (m1, ms' @ m1x :: ms, x))
      | CNamed n =>
        (case IM.find (cons, n) of
             NONE => c
           | SOME nx => CModProj (m1, ms', nx))
      | CApp (c1, c2) => CApp ((sgnS_con' arg (#1 c1), #2 c1),
                               (sgnS_con' arg (#1 c2), #2 c2))
      | _ => c

fun sgnS_class_name (arg as (m1, ms', (sgns, strs, cons))) nm =
    case nm of
        ClProj (m1, ms, x) =>
        (case IM.find (strs, m1) of
             NONE => nm
           | SOME m1x => ClProj (m1, ms' @ m1x :: ms, x))
      | ClNamed n =>
        (case IM.find (cons, n) of
             NONE => nm
           | SOME nx => ClProj (m1, ms', nx))

fun sgnS_class_key (arg as (m1, ms', (sgns, strs, cons))) k =
    case k of
        CkProj (m1, ms, x) =>
        (case IM.find (strs, m1) of
             NONE => k
           | SOME m1x => CkProj (m1, ms' @ m1x :: ms, x))
      | CkNamed n =>
        (case IM.find (cons, n) of
             NONE => k
           | SOME nx => CkProj (m1, ms', nx))
      | CkApp (k1, k2) => CkApp (sgnS_class_key arg k1,
                                 sgnS_class_key arg k2)
      | _ => k

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
              | SOME sgn => sgn
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
                                  (enrichClasses env classes (m1, ms @ [x]) sgn,
                                   newClasses,
                                   sgiSeek (#1 sgi, fmap),
                                   env)
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
                                     | SOME (cn, rule) =>
                                       let
                                           val globalizeN = sgnS_class_name (m1, ms, fmap)
                                           val globalize = sgnS_class_key (m1, ms, fmap)

                                           fun unravel c =
                                               case c of
                                                   ClNamed n =>
                                                   ((case lookupCNamed env n of
                                                         (_, _, SOME c') =>
                                                         (case class_name_in c' of
                                                              NONE => c
                                                            | SOME k => unravel k)
                                                       | _ => c)
                                                    handle UnboundNamed _ => c)
                                                 | _ => c

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
                                                               val e = (EModProj (m1, ms, x),
                                                                                     #2 sgn)

                                                               val class =
                                                                   case rule of
                                                                       Normal (nvars, hyps, a) =>
                                                                       {ground = 
                                                                        KM.insert (#ground class, (globalize a, nvars),
                                                                                   (map (fn (n, k) =>
                                                                                            (globalizeN n,
                                                                                             globalize k)) hyps, e)),
                                                                        inclusions = #inclusions class}
                                                                     | Inclusion f' =>
                                                                       {ground = #ground class,
                                                                        inclusions = inclusion (classes, cn,
                                                                                                #inclusions class,
                                                                                                globalizeN f', e)}
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
                                                                   case rule of
                                                                       Normal (nvars, hyps, a) =>
                                                                       {ground =
                                                                        KM.insert (#ground class, (globalize a, nvars),
                                                                                   (map (fn (n, k) =>
                                                                                            (globalizeN n,
                                                                                             globalize k)) hyps, e)),
                                                                        inclusions = #inclusions class}
                                                                     | Inclusion f' =>
                                                                       {ground = #ground class,
                                                                        inclusions = inclusion (classes, cn,
                                                                                                #inclusions class,
                                                                                                globalizeN f', e)}
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
      | SgiDatatype (x, n, xs, xncs) =>
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

      | SgiClassAbs (x, n, k) => pushCNamedAs env x n (KArrow (k, (KType, loc)), loc) NONE
      | SgiClass (x, n, k, c) => pushCNamedAs env x n (KArrow (k, (KType, loc)), loc) (SOME c)

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
                        | SgiDatatype (x, _, xs, _) =>
                          if x = field then
                              let
                                  val k = (KType, #2 sgn)
                                  val k' = foldl (fn (_, k') => (KArrow (k, k'), #2 sgn)) k xs
                              in
                                  SOME (k', NONE)
                              end
                          else
                              NONE
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
                                                       SOME ((KArrow (k, (KType, #2 sgn)), #2 sgn), NONE)
                                                   else
                                                       NONE
                        | SgiClass (x, _, k, c) => if x = field then
                                                       SOME ((KArrow (k, (KType, #2 sgn)), #2 sgn), SOME c)
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
        (case sgnSeek (fn SgiDatatype (x, _, xs, xncs) => if x = field then SOME (xs, xncs) else NONE
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
            case sgnSeek (fn SgiDatatype (_, n, xs, xncs) => consider (n, xs, xncs)
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
                           | SgiDatatype (_, n, xs, xncs) => seek (n, xs, xncs)
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
                  | SgiDatatype (x, n, _, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
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

fun edeclBinds env (d, loc) =
    case d of
        EDVal (x, t, _) => pushERel env x t
      | EDValRec vis => foldl (fn ((x, t, _), env) => pushERel env x t) env vis

fun declBinds env (d, loc) =
    case d of
        DCon (x, n, k, c) => pushCNamedAs env x n k (SOME c)
      | DDatatype (x, n, xs, xncs) =>
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
      | DTable (tn, x, n, c, _) =>
        let
            val t = (CApp ((CModProj (tn, [], "sql_table"), loc), c), loc)
        in
            pushENamedAs env x n t
        end
      | DSequence (tn, x, n) =>
        let
            val t = (CModProj (tn, [], "sql_sequence"), loc)
        in
            pushENamedAs env x n t
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

fun patBinds env (p, loc) =
    case p of
        PWild => env
      | PVar (x, t) => pushERel env x t
      | PPrim _ => env
      | PCon (_, _, _, NONE) => env
      | PCon (_, _, _, SOME p) => patBinds env p
      | PRecord xps => foldl (fn ((_, p, _), env) => patBinds env p) env xps

end
