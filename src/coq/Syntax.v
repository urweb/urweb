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

Require Import Name.
Export Name.

Set Implicit Arguments.


(** Syntax of Featherweight Ur *)

Inductive kind : Type :=
| KType : kind
| KName : kind
| KArrow : kind -> kind -> kind
| KRecord : kind -> kind.

Section vars.
  Variable cvar : kind -> Type.

  Inductive con : kind -> Type :=
  | CVar : forall k, cvar k -> con k
  | Arrow : con KType -> con KType -> con KType
  | Poly : forall k, (cvar k -> con KType) -> con KType
  | CAbs : forall k1 k2, (cvar k1 -> con k2) -> con (KArrow k1 k2)
  | CApp : forall k1 k2, con (KArrow k1 k2) -> con k1 -> con k2
  | Name : name -> con KName
  | TRecord : con (KRecord KType) -> con KType
  | CEmpty : forall k, con (KRecord k)
  | CSingle : forall k, con KName -> con k -> con (KRecord k)
  | CConcat : forall k, con (KRecord k) -> con (KRecord k) -> con (KRecord k)
  | CMap : forall k1 k2, con (KArrow (KArrow k1 k2) (KArrow (KRecord k1) (KRecord k2)))
  | TGuarded : forall k, con (KRecord k) -> con (KRecord k) -> con KType -> con KType.

  Variable dvar : forall k, con (KRecord k) -> con (KRecord k) -> Type.

  Section subs.
    Variable k1 : kind.
    Variable c1 : con k1.

    Inductive subs : forall k2, (cvar k1 -> con k2) -> con k2 -> Type :=
    | S_Unchanged : forall k2 (c2 : con k2),
      subs (fun _ => c2) c2
    | S_CVar : subs (fun x => CVar x) c1
    | S_Arrow : forall c2 c3 c2' c3',
      subs c2 c2'
      -> subs c3 c3'
      -> subs (fun x => Arrow (c2 x) (c3 x)) (Arrow c2' c3')
    | S_Poly : forall k (c2 : cvar k1 -> cvar k -> _) (c2' : cvar k -> _),
      (forall x', subs (fun x => c2 x x') (c2' x'))
      -> subs (fun x => Poly (c2 x)) (Poly c2')
    | S_CAbs : forall k2 k3 (c2 : cvar k1 -> cvar k2 -> con k3) (c2' : cvar k2 -> _),
      (forall x', subs (fun x => c2 x x') (c2' x'))
      -> subs (fun x => CAbs (c2 x)) (CAbs c2')
    | S_CApp : forall k1 k2 (c2 : _ -> con (KArrow k1 k2)) c3 c2' c3',
      subs c2 c2'
      -> subs c3 c3'
      -> subs (fun x => CApp (c2 x) (c3 x)) (CApp c2' c3')
    | S_TRecord : forall c2 c2',
      subs c2 c2'
      -> subs (fun x => TRecord (c2 x)) (TRecord c2')
    | S_CSingle : forall k2 c2 (c3 : _ -> con k2) c2' c3',
      subs c2 c2'
      -> subs c3 c3'
      -> subs (fun x => CSingle (c2 x) (c3 x)) (CSingle c2' c3')
    | S_CConcat : forall k2 (c2 c3 : _ -> con (KRecord k2)) c2' c3',
      subs c2 c2'
      -> subs c3 c3'
      -> subs (fun x => CConcat (c2 x) (c3 x)) (CConcat c2' c3')
    | S_TGuarded : forall k2 (c2 c3 : _ -> con (KRecord k2)) c4 c2' c3' c4',
      subs c2 c2'
      -> subs c3 c3'
      -> subs c4 c4'
      -> subs (fun x => TGuarded (c2 x) (c3 x) (c4 x)) (TGuarded c2' c3' c4').
  End subs.

  Inductive disj : forall k, con (KRecord k) -> con (KRecord k) -> Prop :=
  | DVar : forall k (c1 c2 : con (KRecord k)),
    dvar c1 c2 -> disj c1 c2
  | DComm : forall k (c1 c2 : con (KRecord k)),
    disj c1 c2 -> disj c2 c1

  | DEmpty : forall k c2,
    disj (CEmpty k) c2
  | DSingleKeys : forall k X1 X2 (c1 c2 : con k),
    X1 <> X2
    -> disj (CSingle (Name X1) c1) (CSingle (Name X2) c2)
  | DSingleValues : forall k n1 n2 (c1 c2 : con k) k' (c1' c2' : con k'),
    disj (CSingle n1 c1') (CSingle n2 c2')
    -> disj (CSingle n1 c1) (CSingle n2 c2)

  | DConcat : forall k (c1 c2 c : con (KRecord k)),
    disj c1 c
    -> disj c2 c
    -> disj (CConcat c1 c2) c

  | DEq : forall k (c1 c2 c1' : con (KRecord k)),
    disj c1 c2
    -> deq c1' c1
    -> disj c1' c2

  with deq : forall k, con k -> con k -> Prop :=
  | Eq_Beta : forall k1 k2 (c1 : cvar k1 -> con k2) c2 c1',
    subs c2 c1 c1'
    -> deq (CApp (CAbs c1) c2) c1'
  | Eq_Refl : forall k (c : con k),
    deq c c
  | Eq_Comm : forall k (c1 c2 : con k),
    deq c2 c1
    -> deq c1 c2
  | Eq_Trans : forall k (c1 c2 c3 : con k),
    deq c1 c2
    -> deq c2 c3
    -> deq c1 c3
  | Eq_Cong : forall k1 k2 c1 c1' (c2 : cvar k1 -> con k2) c2' c2'',
    deq c1 c1'
    -> subs c1 c2 c2'
    -> subs c1' c2 c2''
    -> deq c2' c2''

  | Eq_Concat_Empty : forall k c,
    deq (CConcat (CEmpty k) c) c
  | Eq_Concat_Comm : forall k (c1 c2 c3 : con (KRecord k)),
    disj c1 c2
    -> deq (CConcat c1 c2) (CConcat c2 c1)
  | Eq_Concat_Assoc : forall k (c1 c2 c3 : con (KRecord k)),
    deq (CConcat c1 (CConcat c2 c3)) (CConcat (CConcat c1 c2) c3)

  | Eq_Map_Empty : forall k1 k2 f,
    deq (CApp (CApp (CMap k1 k2) f) (CEmpty _)) (CEmpty _)
  | Eq_Map_Cons : forall k1 k2 f c1 c2 c3,
    disj (CSingle c1 c2) c3
    -> deq (CApp (CApp (CMap k1 k2) f) (CConcat (CSingle c1 c2) c3))
    (CConcat (CSingle c1 (CApp f c2)) (CApp (CApp (CMap k1 k2) f) c3))

  | Eq_Map_Ident : forall k c,
    deq (CApp (CApp (CMap k k) (CAbs (fun x => CVar x))) c) c
  | Eq_Map_Dist : forall k1 k2 f c1 c2,
    deq (CApp (CApp (CMap k1 k2) f) (CConcat c1 c2))
    (CConcat (CApp (CApp (CMap k1 k2) f) c1) (CApp (CApp (CMap k1 k2) f) c2))
  | Eq_Map_Fuse : forall k1 k2 k3 f f' c,
    deq (CApp (CApp (CMap k2 k3) f')
      (CApp (CApp (CMap k1 k2) f) c))
    (CApp (CApp (CMap k1 k3) (CAbs (fun x => CApp f' (CApp f (CVar x))))) c).

  Variable evar : con KType -> Type.

  Inductive exp : con KType -> Type :=
  | Var : forall t, evar t -> exp t
  | App : forall dom ran, exp (Arrow dom ran) -> exp dom -> exp ran
  | Abs : forall dom ran, (evar dom -> exp ran) -> exp (Arrow dom ran)
  | ECApp : forall k (dom : con k) ran ran', exp (Poly ran) -> subs dom ran ran' -> exp ran'
  | ECAbs : forall k (ran : cvar k -> _), (forall X, exp (ran X)) -> exp (Poly ran)
  | Cast : forall t1 t2, deq t1 t2 -> exp t1 -> exp t2
  | Empty : exp (TRecord (CEmpty _))
  | Single : forall c t, exp t -> exp (TRecord (CConcat (CSingle c t) (CEmpty _)))
  | Proj : forall c t c', exp (TRecord (CConcat (CSingle c t) c')) -> exp t
  | Cut : forall c t c', disj (CSingle c t) c' -> exp (TRecord (CConcat (CSingle c t) c')) -> exp (TRecord c')
  | Concat : forall c1 c2, exp (TRecord c1) -> exp (TRecord c2) -> exp (TRecord (CConcat c1 c2))
  | Guarded : forall k (c1 c2 : con (KRecord k)) c, (dvar c1 c2 -> exp c) -> exp (TGuarded c1 c2 c)
  | GuardedApp : forall k (c1 c2 : con (KRecord k)) t, exp (TGuarded c1 c2 t) -> disj c1 c2 -> exp t.
End vars.
