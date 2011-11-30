(* Copyright (c) 2009, 2011, Adam Chlipala
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

Require Import Eqdep_dec.

Require Import Axioms.
Require Import Syntax.

Set Implicit Arguments.


Definition row (A : Type) : Type := name -> option A.

Definition record (r : row Set) := forall n, match r n with
                                               | None => unit
                                               | Some T => T
                                             end.

Fixpoint kDen (k : kind) : Type :=
  match k with
    | KType => Set
    | KName => name
    | KArrow k1 k2 => kDen k1 -> kDen k2
    | KRecord k1 => row (kDen k1)
  end.

Definition disjoint T (r1 r2 : row T) :=
  forall n, match r1 n, r2 n with
              | Some _, Some _ => False
              | _, _ => True
            end.

Fixpoint cDen k (c : con kDen k) : kDen k :=
  match c with
    | CVar _ x => x
    | Arrow c1 c2 => cDen c1 -> cDen c2
    | Poly _ c1 => forall x, cDen (c1 x)
    | CAbs _ _ c1 => fun x => cDen (c1 x)
    | CApp _ _ c1 c2 => (cDen c1) (cDen c2)
    | Name n => n
    | TRecord c1 => record (cDen c1)
    | CEmpty _ => fun _ => None
    | CSingle _ c1 c2 => fun n => if name_eq_dec n (cDen c1) then Some (cDen c2) else None
    | CConcat _ c1 c2 => fun n => match (cDen c1) n with
                                    | None => (cDen c2) n
                                    | v => v
                                  end
    | CMap k1 k2 => fun f r n => match r n with
                                   | None => None
                                   | Some T => Some (f T)
                                 end
    | TGuarded _ c1 c2 t => disjoint (cDen c1) (cDen c2) -> cDen t
  end.

Theorem subs_correct : forall k1 (c1 : con kDen k1) k2 (c2 : _ -> con kDen k2) c2',
  subs c1 c2 c2'
  -> cDen (c2 (cDen c1)) = cDen c2'.
  induction 1; simpl; intuition; try (apply ext_eq_forallS || apply ext_eq);
    repeat match goal with
             | [ H : _ |- _ ] => rewrite H
           end; intuition.
Qed.

Definition dvar k (c1 c2 : con kDen (KRecord k)) :=
  disjoint (cDen c1) (cDen c2).

Scheme deq_mut := Minimality for deq Sort Prop
with disj_mut := Minimality for disj Sort Prop.

Ltac deq_disj_correct scm :=
  let t := repeat progress (simpl; intuition; subst) in

  let rec use_disjoint' notDone E :=
    match goal with
      | [ H : disjoint _ _ |- _ ] =>
        notDone H; generalize (H E); use_disjoint'
          ltac:(fun H' =>
            match H' with
              | H => fail 1
              | _ => notDone H'
            end) E
      | _ => idtac
    end in
  let use_disjoint := use_disjoint' ltac:(fun _ => idtac) in

  apply (scm _ dvar
    (fun k (c1 c2 : con kDen k) =>
      cDen c1 = cDen c2)
    (fun k (c1 c2 : con kDen (KRecord k)) =>
      disjoint (cDen c1) (cDen c2))); t;
  repeat ((unfold row; apply ext_eq)
    || (match goal with
          | [ H : _ |- _ ] => rewrite H; []
          | [ H : subs _ _ _ |- _ ] => rewrite <- (subs_correct H)
        end); t);
  unfold disjoint; t;
    repeat (match goal with
              | [ |- context[match cDen ?C ?E with Some _ => _ | None => _ end] ] =>
                use_disjoint E; destruct (cDen C E)
              | [ |- context[if name_eq_dec ?N1 ?N2 then _ else _] ] =>
                use_disjoint N1; use_disjoint N2; destruct (name_eq_dec N1 N2)
              | [ _ : context[match cDen ?C ?E with Some _ => _ | None => _ end] |- _ ] =>
                use_disjoint E; destruct (cDen C E)
              | [ |- context[if ?E then _ else _] ] => destruct E                  
            end; t).

Hint Unfold dvar.

Theorem deq_correct : forall k (c1 c2 : con kDen k),
  deq dvar c1 c2
  -> cDen c1 = cDen c2.
  deq_disj_correct deq_mut.
Qed.

Theorem disj_correct : forall k (c1 c2 : con kDen (KRecord k)),
  disj dvar c1 c2
  -> disjoint (cDen c1) (cDen c2).
  deq_disj_correct disj_mut.
Qed.

Definition tDen (t : con kDen KType) : Set := cDen t.

Theorem name_eq_dec_refl : forall n, name_eq_dec n n = left _ (refl_equal n).
  intros; destruct (name_eq_dec n n); intuition; [
    match goal with
      | [ e : _ = _ |- _ ] => rewrite (UIP_dec name_eq_dec e (refl_equal _)); reflexivity
    end
    | elimtype False; tauto
  ].
Qed.

Theorem cut_disjoint : forall n1 v r,
  disjoint (fun n => if name_eq_dec n n1 then Some v else None) r
  -> unit = match r n1 with
              | Some T => T
              | None => unit
            end.
  intros;
    match goal with
      | [ H : disjoint _ _ |- _ ] => generalize (H n1)
    end; rewrite name_eq_dec_refl;
    destruct (r n1); intuition.
Qed.

Implicit Arguments cut_disjoint [v r].

Fixpoint eDen t (e : exp dvar tDen t) : tDen t :=
  match e in exp _ _ t return tDen t with
    | Var _ x => x
    | App _ _ e1 e2 => (eDen e1) (eDen e2)
    | Abs _ _ e1 => fun x => eDen (e1 x)
    | ECApp _ c _ _ e1 Hsub => match subs_correct Hsub in _ = T return T with
                                 | refl_equal => (eDen e1) (cDen c)
                               end
    | ECAbs _ _ e1 => fun X => eDen (e1 X)
    | Cast _ _ Heq e1 => match deq_correct Heq in _ = T return T with
                           | refl_equal => eDen e1
                         end
    | Empty => fun _ => tt
    | Single c c' e1 => fun n => if name_eq_dec n (cDen c) as B
      return (match (match (if B then _ else _) with Some _ => _ | None => _ end)
                with Some _ => _ | None => unit end)
      then eDen e1 else tt
    | Proj c _ _ e1 =>
      match name_eq_dec_refl (cDen c) in _ = B
        return (match (match (if B then _ else _) with
                         | Some _ => _
                         | None => _ end)
        return Set
                  with Some _ => _ | None => _ end) with
        | refl_equal => (eDen e1) (cDen c)
      end
    | Cut c _ c' Hdisj e1 => fun n =>
      match name_eq_dec n (cDen c) as B return (match (match (if B then Some _ else None) with Some _ => _ | None => (cDen c') n end)
                                                  with Some T => T | None => unit end
      -> match (cDen c') n with
           | None => unit
           | Some T => T
         end) with 
        | left Heq => fun _ =>
          match sym_eq Heq in _ = n' return match cDen c' n' return Set with Some _ => _ | None => _ end with
            | refl_equal =>
              match cut_disjoint _ (disj_correct Hdisj) in _ = T return T with
                | refl_equal => tt
              end
          end
        | right _ => fun x => x
      end ((eDen e1) n)

    | Concat c1 c2 e1 e2 => fun n =>
      match (cDen c1) n as D return match D with
                                      | None => unit
                                      | Some T => T
                                    end
      -> match (match D with
                  | None => (cDen c2) n
                  | Some v => Some v
                end) with
           | None => unit
           | Some T => T
         end with
        | None => fun _ => (eDen e2) n
        | _ => fun x => x
      end ((eDen e1) n)

    | Guarded _ _ _ _ e1 => fun pf => eDen (e1 pf)
    | GuardedApp _ _ _ _ e1 Hdisj => (eDen e1) (disj_correct Hdisj)
  end.
