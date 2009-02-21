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

Set Implicit Arguments.


Fixpoint name' (n : nat) : Type :=
  match n with
    | O => Empty_set
    | S n' => option (name' n')
  end.

Definition name'_eq_dec : forall n (x y : name' n), {x = y} + {x <> y}.
  Hint Extern 1 (_ = _ -> False) => congruence.

  induction n; simpl; intuition;
    repeat match goal with
             | [ x : Empty_set |- _ ] => destruct x
             | [ x : option _ |- _ ] => destruct x
           end; intuition;
    match goal with
      | [ IH : _, n1 : name' _, n2 : name' _ |- _ ] =>
        destruct (IHn n1 n0); subst; intuition
    end.
Qed.

Definition badName' n (P : name' n -> bool) :
  {nm : _ | P nm = false} + {forall nm, P nm = true}.
  Hint Constructors sig.
  Hint Extern 1 (_ = true) =>
    match goal with
      | [ nm : option _ |- _ ] => destruct nm
    end; auto.

  induction n; simpl; intuition;
    match goal with
      | [ IH : forall P : _ -> _,_ |- _ ] =>
        case_eq (P None);
        destruct (IH (fun nm => P (Some nm))); firstorder eauto
    end.
Qed.

Parameter numNames : nat.
Definition name := name' (S numNames).
Definition name_eq_dec : forall (x y : name), {x = y} + {x <> y} := @name'_eq_dec _.
Definition badName : forall P : name -> bool, {nm : _ | P nm = false} + {forall nm, P nm = true} := @badName' _.
Definition defaultName : name := None.
