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

structure ElabOps :> ELAB_OPS = struct

open Elab

structure E = ElabEnv
structure U = ElabUtil

val liftConInCon = E.liftConInCon

val subConInCon =
    U.Con.mapB {kind = fn k => k,
                con = fn (xn, rep) => fn c =>
                                  case c of
                                      CRel xn' =>
                                      (case Int.compare (xn', xn) of
                                           EQUAL => #1 rep
                                         | GREATER => CRel (xn' - 1)
                                         | LESS => c)
                                    (*| CUnif _ => raise SynUnif*)
                                    | _ => c,
                bind = fn ((xn, rep), U.Con.Rel _) => (xn+1, liftConInCon 0 rep)
                        | (ctx, _) => ctx}

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


fun hnormCon env (cAll as (c, loc)) =
    case c of
        CUnif (_, _, _, ref (SOME c)) => hnormCon env c

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
           | c1' as CApp (c', i) =>
             let
                 fun default () = (CApp ((c1', loc), hnormCon env c2), loc)
             in
                 case #1 (hnormCon env c') of
                     CApp (c', f) =>
                     (case #1 (hnormCon env c') of
                          CFold ks =>
                          (case #1 (hnormCon env c2) of
                               CRecord (_, []) => hnormCon env i
                             | CRecord (k, (x, c) :: rest) =>
                               hnormCon env
                                        (CApp ((CApp ((CApp (f, x), loc), c), loc),
                                               (CApp ((CApp ((CApp ((CFold ks, loc), f), loc), i), loc),
                                                      (CRecord (k, rest), loc)), loc)), loc)
                             | CConcat ((CRecord (k, (x, c) :: rest), _), rest') =>
                               let
                                   val rest'' = (CConcat ((CRecord (k, rest), loc), rest'), loc)

                               (*val ccc = (CApp ((CApp ((CApp (f, x), loc), c), loc),
                                                  (CApp ((CApp ((CApp ((CFold ks, loc), f), loc), i), loc),
                                                         rest''), loc)), loc)*)
                               in
                                   (*eprefaces "Red to" [("ccc", p_con env ccc), ("ccc'", p_con env (hnormCon env ccc))];*)
                                   hnormCon env
                                            (CApp ((CApp ((CApp (f, x), loc), c), loc),
                                                   (CApp ((CApp ((CApp ((CFold ks, loc), f), loc), i), loc),
                                                          rest''), loc)), loc)
                               end
                             | _ =>
                               let
                                   fun cunif () =
                                       let
                                           val r = ref NONE
                                       in
                                           (r, (CUnif (loc, (KType, loc), "_", r), loc))
                                       end

                                   val (nmR, nm) = cunif ()
                                   val (vR, v) = cunif ()
                                   val (rR, r) = cunif ()

                                   val c = f
                                   val c = (CApp (c, nm), loc)
                                   val c = (CApp (c, v), loc)
                                   val c = (CApp (c, r), loc)
                                   fun unconstraint c =
                                       case hnormCon env c of
                                           (CDisjoint (_, _, _, c), _) => unconstraint c
                                         | c => c
                                   val c = unconstraint c

                                   fun tryDistributivity () =
                                       let
                                           fun distribute (c1, c2) =
                                               let
                                                   val c = (CFold ks, loc)
                                                   val c = (CApp (c, f), loc)
                                                   val c = (CApp (c, i), loc)

                                                   val c1 = (CApp (c, c1), loc)
                                                   val c2 = (CApp (c, c2), loc)
                                                   val c = (CConcat (c1, c2), loc)
                                               in
                                                   hnormCon env c
                                               end
                                       in
                                           case (hnormCon env i, hnormCon env c2, hnormCon env c) of
                                               ((CRecord (_, []), _),
                                                (CConcat (arg1, arg2), _),
                                                (CConcat (c1, c2'), _)) =>
                                               (case (hnormCon env c1, hnormCon env c2') of
                                                    ((CRecord (_, [(nm', v')]), _),
                                                     (CUnif (_, _, _, rR'), _)) =>
                                                    (case hnormCon env nm' of
                                                         (CUnif (_, _, _, nmR'), _) =>
                                                         if nmR' = nmR andalso rR' = rR then
                                                             distribute (arg1, arg2)
                                                         else
                                                             default ()
                                                       | _ => default ())
                                                  | _ => default ())
                                             | _ => default ()
                                       end

                                   fun tryFusion () =
                                       let
                                           fun fuse (dom, new_v, r') =
                                               let
                                                   val ran = #2 ks

                                                   val f = (CApp (f, (CRel 2, loc)), loc)
                                                   val f = (CApp (f, new_v), loc)
                                                   val f = (CApp (f, (CRel 0, loc)), loc)
                                                   val f = (CAbs ("acc", ran, f), loc)
                                                   val f = (CAbs ("v", dom, f), loc)
                                                   val f = (CAbs ("nm", (KName, loc), f), loc)

                                                   val c = (CFold (dom, ran), loc)
                                                   val c = (CApp (c, f), loc)
                                                   val c = (CApp (c, i), loc)
                                                   val c = (CApp (c, r'), loc)
                                               in
                                                   hnormCon env c
                                               end
                                       in
                                           case #1 (hnormCon env c2) of
                                               CApp (f, r') =>
                                               (case #1 (hnormCon env f) of
                                                    CApp (f, inner_i) =>
                                                    (case (#1 (hnormCon env f), #1 (hnormCon env inner_i)) of
                                                         (CApp (f, inner_f), CRecord (_, [])) =>
                                                         (case #1 (hnormCon env f) of
                                                              CFold (dom, _) =>
                                                              let
                                                                  val c = inner_f
                                                                  val c = (CApp (c, nm), loc)
                                                                  val c = (CApp (c, v), loc)
                                                                  val c = (CApp (c, r), loc)
                                                                  val c = unconstraint c

                                                                  (*val () = Print.prefaces "Onto something!"
                                                                           [("c", ElabPrint.p_con env cAll),
                                                                            ("c'", ElabPrint.p_con env c)]*)

                                                              in
                                                                  case #1 (hnormCon env c) of
                                                                      CConcat (first, rest) =>
                                                                      (case (#1 (hnormCon env first),
                                                                             #1 (hnormCon env rest)) of
                                                                           (CRecord (_, [(nm', v')]),
                                                                            CUnif (_, _, _, rR')) =>
                                                                           (case #1 (hnormCon env nm') of
                                                                                CUnif (_, _, _, nmR') =>
                                                                                if rR' = rR andalso nmR' = nmR then
                                                                                    (nmR := SOME (CRel 2, loc);
                                                                                     vR := SOME (CRel 1, loc);
                                                                                     rR := SOME (CError, loc);
                                                                                     fuse (dom, v', r'))
                                                                                else
                                                                                    tryDistributivity ()
                                                                              | _ => tryDistributivity ())
                                                                         | _ => tryDistributivity ())
                                                                    | _ => tryDistributivity ()
                                                              end
                                                            | _ => tryDistributivity ())
                                                       | _ => tryDistributivity ())
                                                  | _ => tryDistributivity ())
                                             | _ => tryDistributivity ()
                                       end

                               in
                                   (*Print.prefaces "Consider" [("c", ElabPrint.p_con env c)];*)
                                   case (hnormCon env i, unconstraint c) of
                                       ((CRecord (_, []), _),
                                        (CConcat (c1, c2'), _)) =>
                                       (case (hnormCon env c1, hnormCon env c2') of
                                            ((CRecord (_, [(nm', v')]), _),
                                             (CUnif (_, _, _, rR'), _)) =>
                                            (case (hnormCon env nm', hnormCon env v') of
                                                 ((CUnif (_, _, _, nmR'), _),
                                                  (CUnif (_, _, _, vR'), _)) =>
                                                 if nmR' = nmR andalso vR' = vR andalso rR' = rR then
                                                     hnormCon env c2
                                                 else
                                                     tryFusion ()
                                               | _ => tryFusion ())
                                          | _ => tryFusion ())
                                     | _ => tryFusion ()
                               end)
                        | _ => default ())
                   | _ => default ()
             end
           | c1' => (CApp ((c1', loc), hnormCon env c2), loc))

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

end
