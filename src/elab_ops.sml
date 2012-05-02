(* Copyright (c) 2008, 2012, Adam Chlipala
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

structure ElabOps :> ELAB_OPS = struct

open Elab

structure E = ElabEnv
structure U = ElabUtil

fun liftKindInKind' by =
    U.Kind.mapB {kind = fn bound => fn k =>
                                       case k of
                                         KRel xn =>
                                         if xn < bound then
                                             k
                                         else
                                             KRel (xn + by)
                                       | _ => k,
                bind = fn (bound, _) => bound + 1}

fun subKindInKind' rep =
    U.Kind.mapB {kind = fn (by, xn) => fn k =>
                                          case k of
                                              KRel xn' =>
                                              (case Int.compare (xn', xn) of
                                                   EQUAL => #1 (liftKindInKind' by 0 rep)
                                                 | GREATER => KRel (xn' - 1)
                                                 | LESS => k)
                                            | _ => k,
                 bind = fn ((by, xn), _) => (by+1, xn+1)}

val liftKindInKind = liftKindInKind' 1

fun subKindInKind (xn, rep) = subKindInKind' rep (0, xn)

fun liftKindInCon by =
    U.Con.mapB {kind = fn bound => fn k =>
                                      case k of
                                          KRel xn =>
                                          if xn < bound then
                                              k
                                          else
                                              KRel (xn + by)
                                        | _ => k,
                con = fn _ => fn c => c,
                bind = fn (bound, U.Con.RelK _) => bound + 1
                        | (bound, _) => bound}

fun subKindInCon' rep =
    U.Con.mapB {kind = fn (by, xn) => fn k =>
                                         case k of
                                             KRel xn' =>
                                             (case Int.compare (xn', xn) of
                                                  EQUAL => #1 (liftKindInKind' by 0 rep)
                                                | GREATER => KRel (xn' - 1)
                                                | LESS => k)
                                           | _ => k,
                con = fn _ => fn c => c,
                bind = fn ((by, xn), U.Con.RelK _) => (by+1, xn+1)
                        | (st, _) => st}

val liftKindInCon = liftKindInCon 1

fun subKindInCon (xn, rep) = subKindInCon' rep (0, xn)

fun liftConInCon by =
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
                        | (bound, _) => bound}

exception SubUnif

fun subConInCon' rep =
    U.Con.mapB {kind = fn _ => fn k => k,
                con = fn (by, xn) => fn c =>
                                        case c of
                                            CRel xn' =>
                                            (case Int.compare (xn', xn) of
                                                 EQUAL => #1 (liftConInCon by 0 rep)
                                               | GREATER => CRel (xn' - 1)
                                               | LESS => c)
                                          | CUnif (0, _, _, _, _) => raise SubUnif
                                          | CUnif (n, loc, k, s, r) => CUnif (n-1, loc, k, s, r)
                                          | _ => c,
                bind = fn ((by, xn), U.Con.RelC _) => (by+1, xn+1)
                        | (ctx, _) => ctx}

val liftConInCon = liftConInCon 1

fun subConInCon (xn, rep) = subConInCon' rep (0, xn)

fun subStrInSgn (m1, m2) =
    U.Sgn.map {kind = fn k => k,
               con = fn c as CModProj (m1', ms, x) =>
                        if m1 = m1' then
                            CModProj (m2, ms, x)
                        else
                            c
                      | c => c,
               sgn_item = fn sgi => sgi,
               sgn = fn sgn => sgn}

val occurs =
    U.Con.existsB {kind = fn _ => false,
                   con = fn (n, c) =>
                            case c of
                                CRel n' => n' = n
                              | _ => false,
                   bind = fn (n, b) =>
                             case b of
                                 U.Con.RelC _ => n + 1
                               | _ => n}
                  0

val identity = ref 0
val distribute = ref 0
val fuse = ref 0

fun reset () = (identity := 0;
                distribute := 0;
                fuse := 0)

fun hnormCon env (cAll as (c, loc)) =
    case c of
        CUnif (nl, _, _, _, ref (Known c)) => (#1 (hnormCon env (E.mliftConInCon nl c)), loc)

      | CNamed xn =>
        (case E.lookupCNamed env xn of
             (_, _, SOME c') => hnormCon env c'
           | _ => cAll)

      | CModProj (n, ms, x) =>
        let
            val (_, sgn) = E.lookupStrNamed env n
            val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                       case E.projectStr env {sgn = sgn, str = str, field = m} of
                                           NONE => raise Fail "hnormCon: Unknown substructure"
                                         | SOME sgn => ((StrProj (str, m), loc), sgn))
                             ((StrVar n, loc), sgn) ms
        in
            case E.projectCon env {sgn = sgn, str = str, field = x} of
                NONE => raise Fail "kindof: Unknown con in structure"
              | SOME (_, NONE) => cAll
              | SOME (_, SOME c) => hnormCon env c
        end

      (* Eta reduction *)
      | CAbs (x, k, b) =>
        (case #1 (hnormCon (E.pushCRel env x k) b) of
             CApp (f, (CRel 0, _)) =>
             if occurs f then
                 cAll
             else
                 hnormCon env (subConInCon (0, (CUnit, loc)) f)
           | _ => cAll)

      | CApp (c1, c2) =>
        (case #1 (hnormCon env c1) of
             CAbs (x, k, cb) =>
             let
                 val sc = (hnormCon env (subConInCon (0, c2) cb))
                     handle SynUnif => cAll
                 (*val env' = E.pushCRel env x k*)
             in
                 (*Print.eprefaces "Subst" [("x", Print.PD.string x),
                                          ("cb", ElabPrint.p_con env' cb),
                                          ("c2", ElabPrint.p_con env c2),
                                          ("sc", ElabPrint.p_con env sc)];*)
                 sc
             end
           | c1' as CApp (c', f) =>
             let
                 fun default () = (CApp ((c1', loc), hnormCon env c2), loc)
             in
                 case #1 (hnormCon env c') of
                     CMap (ks as (k1, k2)) =>
                     (case #1 (hnormCon env c2) of
                          CRecord (_, []) => (CRecord (k2, []), loc)
                        | CRecord (_, (x, c) :: rest) =>
                          hnormCon env
                                   (CConcat ((CRecord (k2, [(x, (CApp (f, c), loc))]), loc),
                                             (CApp (c1, (CRecord (k2, rest), loc)), loc)), loc)
                        | CConcat ((CRecord (k, (x, c) :: rest), _), rest') =>
                          let
                              val rest'' = (CConcat ((CRecord (k, rest), loc), rest'), loc)
                          in
                              hnormCon env
                                       (CConcat ((CRecord (k2, [(x, (CApp (f, c), loc))]), loc),
                                                 (CApp (c1, rest''), loc)), loc)
                          end
                        | _ =>
                          let
                              fun unconstraint c =
                                  case hnormCon env c of
                                      (TDisjoint (_, _, c), _) => unconstraint c
                                    | c => c

                              fun inc r = r := !r + 1

                              fun tryDistributivity () =
                                  case hnormCon env c2 of
                                      (CConcat (c1, c2'), _) =>
                                      let
                                          val c = (CMap ks, loc)
                                          val c = (CApp (c, f), loc)
                                                  
                                          val c1 = (CApp (c, c1), loc)
                                          val c2 = (CApp (c, c2'), loc)
                                          val c = (CConcat (c1, c2), loc)
                                      in
                                          inc distribute;
                                          hnormCon env c
                                      end
                                    | _ => default ()

                              fun tryFusion () =
                                  case #1 (hnormCon env c2) of
                                      CApp (f', r') =>
                                      (case #1 (hnormCon env f') of
                                           CApp (f', inner_f) =>
                                           (case #1 (hnormCon env f') of
                                                CMap (dom, _) =>
                                                let
                                                    val inner_f = liftConInCon 0 inner_f
                                                    val f = liftConInCon 0 f

                                                    val f' = (CApp (inner_f, (CRel 0, loc)), loc)
                                                    val f' = (CApp (f, f'), loc)
                                                    val f' = (CAbs ("v", dom, f'), loc)

                                                    val c = (CMap (dom, k2), loc)
                                                    val c = (CApp (c, f'), loc)
                                                    val c = (CApp (c, r'), loc)
                                                in
                                                    inc fuse;
                                                    hnormCon env c
                                                end
                                              | _ => tryDistributivity ())
                                         | _ => tryDistributivity ())
                                    | _ => tryDistributivity ()

                              fun tryIdentity () =
                                  let
                                      fun cunif () =
                                          let
                                              val r = ref (Unknown (fn _ => true))
                                          in
                                              (r, (CUnif (0, loc, (KType, loc), "_", r), loc))
                                          end
                                          
                                      val (vR, v) = cunif ()

                                      val c = (CApp (f, v), loc)
                                  in
                                      case unconstraint c of
                                          (CUnif (_, _, _, _, vR'), _) =>
                                          if vR' = vR then
                                              (inc identity;
                                               hnormCon env c2)
                                          else
                                              tryFusion ()
                                        | _ => tryFusion ()
                                  end
                          in
                              tryIdentity ()
                          end)
                   | _ => default ()
             end
           | c1' => (CApp ((c1', loc), hnormCon env c2), loc))

      | CKApp (c1, k) =>
        (case hnormCon env c1 of
             (CKAbs (_, body), _) => hnormCon env (subKindInCon (0, k) body)
           | _ => cAll)
        
      | CConcat (c1, c2) =>
        (case (hnormCon env c1, hnormCon env c2) of
             ((CRecord (k, xcs1), loc), (CRecord (_, xcs2), _)) =>
             (CRecord (k, xcs1 @ xcs2), loc)
           | ((CRecord (_, []), _), c2') => c2'
           | ((CConcat (c11, c12), loc), c2') =>
             hnormCon env (CConcat (c11, (CConcat (c12, c2'), loc)), loc)
           | (c1', (CRecord (_, []), _)) => c1'
           | (c1', c2') => (CConcat (c1', c2'), loc))

      | CProj (c, n) =>
        (case hnormCon env c of
             (CTuple cs, _) => hnormCon env (List.nth (cs, n - 1))
           | _ => cAll)

      | _ => cAll

fun reduceCon env (cAll as (c, loc)) =
    case c of
        TFun (c1, c2) => (TFun (reduceCon env c1, reduceCon env c2), loc)
      | TCFun (exp, x, k, c) => (TCFun (exp, x, k, reduceCon env c), loc)
      | TRecord c => (TRecord (reduceCon env c), loc)
      | TDisjoint (c1, c2, c3) => (TDisjoint (reduceCon env c1, reduceCon env c2, reduceCon env c3), loc)

      | CRel _ => cAll
      | CNamed xn =>
        (case E.lookupCNamed env xn of
             (_, _, SOME c') => reduceCon env c'
           | _ => cAll)
      | CModProj (n, ms, x) =>
        let
            val (_, sgn) = E.lookupStrNamed env n
            val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                       case E.projectStr env {sgn = sgn, str = str, field = m} of
                                           NONE => raise Fail "reduceCon: Unknown substructure"
                                         | SOME sgn => ((StrProj (str, m), loc), sgn))
                                   ((StrVar n, loc), sgn) ms
        in
            case E.projectCon env {sgn = sgn, str = str, field = x} of
                NONE => raise Fail "reduceCon: kindof: Unknown con in structure"
              | SOME (_, NONE) => cAll
              | SOME (_, SOME c) => reduceCon env c
        end

      | CApp (c1, c2) =>
        let
            val c1 = reduceCon env c1
            val c2 = reduceCon env c2
            fun default () = (CApp (c1, c2), loc)
        in
            case #1 c1 of
                CAbs (x, k, cb) =>
                ((reduceCon env (subConInCon (0, c2) cb))
                 handle SynUnif => default ())
              | CApp (c', f) =>
                let
                    val c' = reduceCon env c'
                    val f = reduceCon env f
                in
                    case #1 c' of
                        CMap (ks as (k1, k2)) =>
                        (case #1 c2 of
                             CRecord (_, []) => (CRecord (k2, []), loc)
                           | CRecord (_, (x, c) :: rest) =>
                             reduceCon env
                                       (CConcat ((CRecord (k2, [(x, (CApp (f, c), loc))]), loc),
                                                 (CApp (c1, (CRecord (k2, rest), loc)), loc)), loc)
                           | CConcat ((CRecord (k, (x, c) :: rest), _), rest') =>
                             let
                                 val rest'' = (CConcat ((CRecord (k, rest), loc), rest'), loc)
                             in
                                 reduceCon env
                                           (CConcat ((CRecord (k2, [(x, (CApp (f, c), loc))]), loc),
                                                     (CApp (c1, rest''), loc)), loc)
                             end
                           | _ =>
                             let
                                 fun unconstraint c =
                                     case reduceCon env c of
                                         (TDisjoint (_, _, c), _) => unconstraint c
                                       | c => c

                                 fun inc r = r := !r + 1

                                 fun tryDistributivity () =
                                     case reduceCon env c2 of
                                         (CConcat (c1, c2), _) =>
                                         let
                                             val c = (CMap ks, loc)
                                             val c = (CApp (c, f), loc)
                                                  
                                             val c1 = (CApp (c, c1), loc)
                                             val c2 = (CApp (c, c2), loc)
                                             val c = (CConcat (c1, c2), loc)
                                         in
                                             inc distribute;
                                             reduceCon env c
                                         end
                                       | _ => default ()

                                 fun tryFusion () =
                                     case #1 (reduceCon env c2) of
                                         CApp (f', r') =>
                                         (case #1 (reduceCon env f') of
                                              CApp (f', inner_f) =>
                                              (case #1 (reduceCon env f') of
                                                   CMap (dom, _) =>
                                                   let
                                                       val inner_f = liftConInCon 0 inner_f
                                                       val f = liftConInCon 0 f

                                                       val f' = (CApp (inner_f, (CRel 0, loc)), loc)
                                                       val f' = (CApp (f, f'), loc)
                                                       val f' = (CAbs ("v", dom, f'), loc)

                                                       val c = (CMap (dom, k2), loc)
                                                       val c = (CApp (c, f'), loc)
                                                       val c = (CApp (c, r'), loc)
                                                   in
                                                       inc fuse;
                                                       reduceCon env c
                                                   end
                                                 | _ => tryDistributivity ())
                                            | _ => tryDistributivity ())
                                       | _ => tryDistributivity ()

                                 fun tryIdentity () =
                                     let
                                         fun cunif () =
                                             let
                                                 val r = ref (Unknown (fn _ => true))
                                             in
                                                 (r, (CUnif (0, loc, (KType, loc), "_", r), loc))
                                             end
                                             
                                         val (vR, v) = cunif ()

                                         val c = (CApp (f, v), loc)
                                     in
                                         case unconstraint c of
                                             (CUnif (_, _, _, _, vR'), _) =>
                                             if vR' = vR then
                                                 (inc identity;
                                                  reduceCon env c2)
                                             else
                                                 tryFusion ()
                                           | _ => tryFusion ()
                                     end
                             in
                                 tryIdentity ()
                             end)
                      | _ => default ()
                end
              | _ => default ()
        end
      | CAbs (x, k, b) =>
        let
            val b = reduceCon (E.pushCRel env x k) b
            fun default () = (CAbs (x, k, b), loc)
        in
            case #1 b of
                CApp (f, (CRel 0, _)) =>
                if occurs f then
                    default ()
                else
                    reduceCon env (subConInCon (0, (CUnit, loc)) f)
              | _ => default ()
        end

      | CKAbs (x, b) => (CKAbs (x, reduceCon (E.pushKRel env x) b), loc)
      | CKApp (c1, k) =>
        (case reduceCon env c1 of
             (CKAbs (_, body), _) => reduceCon env (subKindInCon (0, k) body)
           | c1 => (CKApp (c1, k), loc))
      | TKFun (x, c) => (TKFun (x, reduceCon env c), loc)

      | CName _ => cAll

      | CRecord (k, xcs) => (CRecord (k, map (fn (x, c) => (reduceCon env x, reduceCon env c)) xcs), loc)
      | CConcat (c1, c2) =>
        let
            val c1 = reduceCon env c1
            val c2 = reduceCon env c2
        in
            case (c1, c2) of
                ((CRecord (k, xcs1), loc), (CRecord (_, xcs2), _)) => (CRecord (k, xcs1 @ xcs2), loc)
              | ((CRecord (_, []), _), _) => c2
              | ((CConcat (c11, c12), loc), _) => reduceCon env (CConcat (c11, (CConcat (c12, c2), loc)), loc)
              | (_, (CRecord (_, []), _)) => c1
              | ((CRecord (k, xcs1), loc), (CConcat ((CRecord (_, xcs2), _), c2'), _)) => (CConcat ((CRecord (k, xcs1 @ xcs2), loc), c2'), loc)
              | _ => (CConcat (c1, c2), loc)
        end
      | CMap _ => cAll

      | CUnit => cAll

      | CTuple cs => (CTuple (map (reduceCon env) cs), loc)
      | CProj (c, n) =>
        (case reduceCon env c of
             (CTuple cs, _) => reduceCon env (List.nth (cs, n - 1))
           | c => (CProj (c, n), loc))

      | CError => cAll

      | CUnif (nl, _, _, _, ref (Known c)) => reduceCon env (E.mliftConInCon nl c)
      | CUnif _ => cAll

end
