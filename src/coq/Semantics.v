(* Copyright (c) 2009, Adam Chlipala
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

Require Import Arith List TheoryList.

Require Import Axioms.
Require Import Syntax.

Set Implicit Arguments.


Definition row (T : Type) := list (name * T).

Fixpoint record (r : row Set) : Set :=
  match r with
    | nil => unit
    | (_, T) :: r' => T * record r'
  end%type.

Fixpoint kDen (k : kind) : Type :=
  match k with
    | KType => Set
    | KName => name
    | KArrow k1 k2 => kDen k1 -> kDen k2
    | KRecord k1 => row (kDen k1)
  end.

Fixpoint cfold T T' (f : name -> T -> T' -> T') (i : T') (r : row T) {struct r} : T' :=
  match r with
    | nil => i
    | (n, v) :: r' => f n v (cfold f i r')
  end.

Fixpoint cDen k (c : con kDen k) {struct c} : kDen k :=
  match c in con _ k return kDen k with
    | CVar _ x => x
    | Arrow c1 c2 => cDen c1 -> cDen c2
    | Poly _ c1 => forall x, cDen (c1 x)
    | CAbs _ _ c1 => fun x => cDen (c1 x)
    | CApp _ _ c1 c2 => (cDen c1) (cDen c2)
    | Name n => n
    | TRecord c1 => record (cDen c1)
    | CEmpty _ => nil
    | CSingle _ c1 c2 => (cDen c1, cDen c2) :: nil
    | CConcat _ c1 c2 => cDen c1 ++ cDen c2
    | CFold k1 k2 => @cfold _ _
    | CGuarded _ _ _ _ c => cDen c
  end.

Theorem subs_correct : forall k1 (c1 : con kDen k1) k2 (c2 : _ -> con kDen k2) c2',
  subs c1 c2 c2'
  -> cDen (c2 (cDen c1)) = cDen c2'.
  induction 1; simpl; intuition; try (apply ext_eq_forallS || apply ext_eq);
    repeat match goal with
             | [ H : _ |- _ ] => rewrite H
           end; intuition.
Qed.

Definition disjoint T (r1 r2 : row T) :=
  AllS (fun p1 => AllS (fun p2 => fst p1 <> fst p2) r2) r1.
Definition dvar k (c1 c2 : con kDen (KRecord k)) :=
  disjoint (cDen c1) (cDen c2).

Lemma AllS_app : forall T P (ls2 : list T),
  AllS P ls2
  -> forall ls1, AllS P ls1
    -> AllS P (ls1 ++ ls2).
  induction 2; simpl; intuition.
Qed.

Lemma AllS_weaken : forall T (P P' : T -> Prop),
  (forall x, P x -> P' x)
  -> forall ls,
    AllS P ls
    -> AllS P' ls.
  induction 2; simpl; intuition.
Qed.

Theorem disjoint_symm : forall T (r1 r2 : row T),
  disjoint r1 r2
  -> disjoint r2 r1.
  Hint Constructors AllS.
  Hint Resolve AllS_weaken.
  
  unfold disjoint; induction r2; simpl; intuition.
  constructor.
  eapply AllS_weaken; eauto.
  intuition.
  inversion H0; auto.

  apply IHr2.
  eapply AllS_weaken; eauto.
  intuition.
  inversion H0; auto.
Qed.  

Lemma map_id : forall k (r : row k),
  cfold (fun x x0 (x1 : row _) => (x, x0) :: x1) nil r = r.
  induction r; simpl; intuition;
    match goal with
      | [ H : _ |- _ ] => rewrite H
    end; intuition.
Qed.

Lemma map_dist : forall T1 T2 (f : T1 -> T2) (r2 r1 : row T1),
  cfold (fun x x0 (x1 : row _) => (x, f x0) :: x1) nil (r1 ++ r2)
  = cfold (fun x x0 (x1 : row _) => (x, f x0) :: x1) nil r1
  ++ cfold (fun x x0 (x1 : row _) => (x, f x0) :: x1) nil r2.
  induction r1; simpl; intuition;
    match goal with
      | [ H : _ |- _ ] => rewrite H
    end; intuition.
Qed.

Lemma fold_fuse : forall T1 T2 T3 (f : name -> T1 -> T2 -> T2) (i : T2) (f' : T3 -> T1) (c : row T3),
  cfold f i (cfold (fun x x0 (x1 : row _) => (x, f' x0) :: x1) nil c)
  = cfold (fun x x0 => f x (f' x0)) i c.
  induction c; simpl; intuition;
    match goal with
      | [ H : _ |- _ ] => rewrite <- H
    end; intuition.
Qed.

Scheme deq_mut := Minimality for deq Sort Prop
with disj_mut := Minimality for disj Sort Prop.

Theorem deq_correct : forall k (c1 c2 : con kDen k),
  deq dvar c1 c2
  -> cDen c1 = cDen c2.
  Hint Resolve map_id map_dist fold_fuse AllS_app disjoint_symm.
  Hint Extern 1 (_ = _) => unfold row; symmetry; apply app_ass.

  apply (deq_mut (dvar := dvar)
    (fun k (c1 c2 : con kDen k) =>
      cDen c1 = cDen c2)
    (fun k (c1 c2 : con kDen (KRecord k)) =>
      disjoint (cDen c1) (cDen c2)));
  simpl; intuition;
    repeat (match goal with
              | [ H : _ |- _ ] => rewrite H
              | [ H : subs _ _ _ |- _ ] => rewrite <- (subs_correct H)
            end; simpl; intuition); try congruence; unfold disjoint in *; intuition;
    fold kDen in *; repeat match goal with
                             | [ H : AllS _ (_ :: _) |- _ ] => inversion H; clear H; subst; simpl in *
                           end; auto.
Qed.
