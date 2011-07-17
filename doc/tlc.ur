(* Chapter 2: Type-Level Computation *)

(* begin hide *)
val show_string = mkShow (fn s => "\"" ^ s ^ "\"")
(* end *)

(* This tutorial by <a href="http://adam.chlipala.net/">Adam Chlipala</a> is licensed under a <a href="http://creativecommons.org/licenses/by-nc-nd/3.0/">Creative Commons Attribution-Noncommercial-No Derivative Works 3.0 Unported License</a>. *)

(* The last chapter reviewed some Ur features imported from ML and Haskell.  This chapter explores uncharted territory, introducing the features that make Ur unique. *)

(* * Names and Records *)

(* Last chapter, we met Ur's basic record features, including record construction and field projection. *)

val r = { A = 0, B = 1.2, C = "hi"}

(* begin eval *)
r.B
(* end *)

(* Our first taste of Ur's novel expressive power is with the following function, which implements record field projection in a completely generic way. *)

fun project [nm :: Name] [t ::: Type] [ts ::: {Type}] [[nm] ~ ts] (r : $([nm = t] ++ ts)) : t = r.nm

(* begin eval *)
project [#B] r
(* end *)

(* This function introduces a slew of essential features.  First, we see type parameters with explicit kind annotations.  Formal parameter syntax like <tt>[a :: K]</tt> declares an <b>explicit</b> parameter <tt>a</tt> of kind <tt>K</tt>.  Explicit parameters must be passed explicitly at call sites.  In contrast, implicit parameters, declared like <tt>[a ::: K]</tt>, are inferred in the usual way.<br>
<br>
Two new kinds appear in this example.  We met the basic kind <tt>Type</tt> in a previous example.  Here we meet <tt>Name</tt>, the kind of record field names; and <tt>{Type}</tt> the type of finite maps from field names to types, where we'll generally refer to this notion of "finite map" by the name <b>record</b>, as it will be clear from context whether we're discussing type-level or value-level records.  That is, in this case, we are referring to names and records <b>at the level of types</b> that <b>exist only at compile time</b>!  By the way, the kind <tt>{Type}</tt> is one example of the general <tt>{K}</tt> kind form, which refers to records with fields of kind <tt>K</tt>.<br>
<br>
The English description of <tt>project</tt> is that it projects a field with name <tt>nm</tt> and type <tt>t</tt> out of a record <tt>r</tt> whose other fields are described by type-level record <tt>ts</tt>.  We make all this formal by assigning <tt>r</tt> a type that first builds the singleton record <tt>[nm = t]</tt> that maps <tt>nm</tt> to <tt>t</tt>, and then concatenating this record with the remaining field information in <tt>ts</tt>.  The <tt>$</tt> operator translates a type-level record (of kind <tt>{Type}</tt>) into a record type (of kind <tt>Type</tt>).<br>
<br>
The type annotation on <tt>r</tt> uses the record concatenation operator <tt>++</tt>.  Ur enforces that any concatenation happens between records that share no field names.  Otherwise, we'd need to resolve field name ambiguity in some predictable way, which would force us to treat <tt>++</tt> as non-commutative, if we are to maintain the nice modularity properties of polymorphism.  However, treating <tt>++</tt> as commutative, and treating records as equal up to field permutation in general, are very convenient for type inference and general programmer experience.  Thus, we enforce disjointness to keep things simple.<br>
<br>
For a polymorphic function like <tt>project</tt>, the compiler doesn't know which fields a type-level record variable like <tt>ts</tt> contains.  To enable self-contained type-checking, we need to declare some constraints about field disjointness.  That's exactly the meaning of syntax like <tt>[r1 ~ r2]</tt>, which asserts disjointness of two type-level records.  The disjointness clause for <tt>project</tt> asserts that the name <tt>nm</tt> is not used by <tt>ts</tt>.  The syntax <tt>[nm]</tt> is shorthand for <tt>[nm = ()]</tt>, which defines a singleton record of kind <tt>{Unit}</tt>, where <tt>Unit</tt> is the degenerate kind inhabited only by the constructor <tt>()</tt>.<br>
<br>
The last piece of this puzzle is the easiest.  In the example call to <tt>project</tt>, we see that the only parameters passed are the one explicit constructor parameter <tt>nm</tt> and the value-level parameter <tt>r</tt>.  The rest are inferred, and the disjointness proof obligation is discharged automatically.  The syntax <tt>#A</tt> denotes the constructor standing for first-class field name <tt>A</tt>, and we pass all constructor parameters to value-level functions within square brackets (which bear no formal relation to the syntax for type-level record literals <tt>[A = c, ..., A = c]</tt>). *)


(* * Basic Type-Level Programming *)

(* To help us express more interesting operations over records, we will need to do some type-level programming.  Ur makes that fairly congenial, since Ur's constructor level includes an embedded copy of the simply-typed lambda calculus.  Here are a few examples. *)

con id = fn t :: Type => t

val x : id int = 0
val x : id float = 1.2

con pair = fn t :: Type => t * t

val x : pair int = (0, 1)
val x : pair float = (1.2, 2.3)

con compose = fn (f :: Type -> Type) (g :: Type -> Type) (t :: Type) => f (g t)

val x : compose pair pair int = ((0, 1), (2, 3))

con fst = fn t :: (Type * Type) => t.1
con snd = fn t :: (Type * Type) => t.2

con p = (int, float)
val x : fst p = 0
val x : snd p = 1.2

con mp = fn (f :: Type -> Type) (t1 :: Type, t2 :: Type) => (f t1, f t2)

val x : fst (mp pair p) = (1, 2)

(* Actually, Ur's constructor level goes further than merely including a copy of the simply-typed lambda calculus with tuples.  We also effectively import classic <b>let-polymorphism</b>, via <b>kind polymorphism</b>, which we can use to make some of the definitions above more generic. *)

con fst = K1 ==> K2 ==> fn t :: (K1 * K2) => t.1
con snd = K1 ==> K2 ==> fn t :: (K1 * K2) => t.2

con twoFuncs :: ((Type -> Type) * (Type -> Type)) = (id, compose pair pair)

val x : fst twoFuncs int = 0
val x : snd twoFuncs int = ((1, 2), (3, 4))


(* * Type-Level Map *)

(* The examples from the same section may seem cute but not especially useful.  In this section, we meet <tt>map</tt>, the real workhorse of Ur's type-level computation.  We'll use it to type some useful operations over value-level records.  A few more pieces will be necessary before getting there, so we'll start just by showing how interesting type-level operations on records may be built from <tt>map</tt>. *)

con r = [A = int, B = float, C = string]

con optionify = map option
val x : $(optionify r) = {A = Some 1, B = None, C = Some "hi"}

con pairify = map pair
val x : $(pairify r) = {A = (1, 2), B = (3.0, 4.0), C = ("5", "6")}

con stringify = map (fn _ => string)
val x : $(stringify r) = {A = "1", B = "2", C = "3"}

(* We'll also give our first hint at the cleverness within Ur's type inference engine.  The following definition type-checks, despite the fact that doing so requires applying several algebraic identities about <tt>map</tt> and <tt>++</tt>.  This is the first point where we see a clear advantage of Ur over the type-level computation facilities that have become popular in GHC Haskell. *)

fun concat [f :: Type -> Type] [r1 :: {Type}] [r2 :: {Type}] [r1 ~ r2]
           (r1 : $(map f r1)) (r2 : $(map f r2)) : $(map f (r1 ++ r2)) = r1 ++ r2


(* * First-Class Polymorphism *)

(* The idea of <b>first-class polymorphism</b> or <b>impredicative polymorphism</b> has also become popular in GHC Haskell.  This feature, which has a long history in type theory, is also central to Ur's metaprogramming facilities.  First-class polymorphism goes beyond Hindley-Milner's let-polymorphism to allow arguments to functions to themselves be polymorphic.  Among other things, this enables the classic example of Church encodings, as for the natural numbers in this example. *)

type nat = t :: Type -> t -> (t -> t) -> t
val zero : nat = fn [t :: Type] (z : t) (s : t -> t) => z
fun succ (n : nat) : nat = fn [t :: Type] (z : t) (s : t -> t) => s (n [t] z s)

val one = succ zero
val two = succ one
val three = succ two

(* begin eval *)
three [int] 0 (plus 1)
(* end *)

(* begin eval *)
three [string] "" (strcat "!")
(* end *)
