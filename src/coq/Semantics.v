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

Require Import Arith List Omega TheoryList.

Require Import Syntax.

Set Implicit Arguments.


Section row'.
  Variable A : Type.

  Inductive row' : list name -> Type :=
  | Nil : row' nil
  | Cons : forall n ls, A -> AllS (lt n) ls -> row' ls -> row' (n :: ls).
End row'.

Implicit Arguments Nil [A].

Record row (A : Type) : Type := Row {
  keys : list name;
  data : row' A keys
}.

Inductive record' : forall ls, row' Set ls -> Set :=
| RNil : record' Nil
| RCons : forall n ls (T : Set) (pf : AllS (lt n) ls) r, T -> record' r -> record' (Cons T pf r).

Definition record (r : row Set) := record' (data r).

Fixpoint kDen (k : kind) : Type :=
  match k with
    | KType => Set
    | KName => name
    | KArrow k1 k2 => kDen k1 -> kDen k2
    | KRecord k1 => row (kDen k1)
  end.

Axiom cheat : forall T, T.

Fixpoint cinsert (n : name) (ls : list name) {struct ls} : list name :=
  match ls with
    | nil => n :: nil
    | n' :: ls' =>
      if eq_nat_dec n n'
        then ls
        else if le_lt_dec n n'
          then n :: ls
          else n' :: cinsert n ls'
  end.

Hint Constructors AllS.
Hint Extern 1 (_ < _) => omega.

Lemma insert_front' : forall n n',
  n <> n'
  -> n <= n'
  -> forall ls, AllS (lt n') ls
    -> AllS (lt n) ls.
  induction 3; auto.
Qed.

Lemma insert_front : forall n n',
  n <> n'
  -> n <= n'
  -> forall ls, AllS (lt n') ls
    -> AllS (lt n) (n' :: ls).
  Hint Resolve insert_front'.
  eauto.
Qed.

Lemma insert_continue : forall n n',
  n <> n'
  -> n' < n
  -> forall ls, AllS (lt n') ls
    -> AllS (lt n') (cinsert n ls).
  induction 3; simpl; auto;
    repeat (match goal with
              | [ |- context[if ?E then _ else _] ] => destruct E
            end; auto).
Qed.

Fixpoint insert T (n : name) (v : T) ls (r : row' T ls) {struct r} : row' T (cinsert n ls) :=
  match r in row' _ ls return row' T (cinsert n ls) with
    | Nil => Cons (n := n) v (allS_nil _) Nil
    | Cons n' ls' v' pf r' =>
      match eq_nat_dec n n' as END
        return row' _ (if END then _ else _) with
        | left _ => Cons (n := n') v' pf r'
        | right pfNe =>
          match le_lt_dec n n' as LLD
            return row' _ (if LLD then _ else _) with
            | left pfLe => Cons (n := n) v (insert_front pfNe pfLe pf) (Cons (n := n') v' pf r')
            | right pfLt => Cons (n := n') v' (insert_continue pfNe pfLt pf) (insert n v r')
          end
      end
  end.

Fixpoint cconcat (ls1 ls2 : list name) {struct ls1} : list name :=
  match ls1 with
    | nil => ls2
    | n :: ls1' => cinsert n (cconcat ls1' ls2)
  end.

Fixpoint concat T ls1 ls2 (r1 : row' T ls1) (r2 : row' T ls2) {struct r1} : row' T (cconcat ls1 ls2) :=
  match r1 in row' _ ls1 return row' _ (cconcat ls1 _) with
    | Nil => r2
    | Cons n _ v _ r1' => insert n v (concat r1' r2)
  end.

Fixpoint cfold T T' (f : name -> T -> T' -> T') (i : T') ls (r : row' T ls) {struct r} : T' :=
  match r with
    | Nil => i
    | Cons n _ v _ r' => f n v (cfold f i r')
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
    | CEmpty _ => Row Nil
    | CSingle _ c1 c2 => Row (Cons (n := cDen c1) (cDen c2) (allS_nil _) Nil)
    | CConcat _ c1 c2 => Row (concat (data (cDen c1)) (data (cDen c2)))
    | CFold k1 k2 => fun f i r => cfold f i (data r)
    | CGuarded _ _ _ _ c => cDen c
  end.
