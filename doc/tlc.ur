(* Chapter 2: Type-Level Computation *)

(* begin hide *)
val show_string = mkShow (fn s => "\"" ^ s ^ "\"")
(* end *)

(* This tutorial by <a href="http://adam.chlipala.net/">Adam Chlipala</a> is licensed under a <a href="http://creativecommons.org/licenses/by-nc-nd/3.0/">Creative Commons Attribution-Noncommercial-No Derivative Works 3.0 Unported License</a>. *)

(* The last chapter reviewed some Ur features imported from ML and Haskell.  This chapter explores uncharted territory, introducing the features that make Ur unique. *)

(* * Names and Records *)

(* Last chapter, we met Ur's basic record features, including record construction and field projection. *)

val r = { A = 0, B = 1.2, C = "hi" }

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
The English description of <tt>project</tt> is that it projects a field with name <tt>nm</tt> and type <tt>t</tt> out of a record <tt>r</tt> whose other fields are described by type-level record <tt>ts</tt>.  We make all this formal by assigning <tt>r</tt> a type that first builds the singleton record <tt>[nm = t]</tt> that maps <tt>nm</tt> to <tt>t</tt>, and then concatenates this record with the remaining field information in <tt>ts</tt>.  The <tt>$</tt> operator translates a type-level record (of kind <tt>{Type}</tt>) into a record type (of kind <tt>Type</tt>).<br>
<br>
The type annotation on <tt>r</tt> uses the record concatenation operator <tt>++</tt>.  Ur enforces that any concatenation happens between records that share no field names.  Otherwise, we'd need to resolve field name ambiguity in some predictable way, which would force us to treat <tt>++</tt> as non-commutative, if we are to maintain the nice modularity properties of polymorphism.  However, treating <tt>++</tt> as commutative, and treating records as equal up to field permutation in general, are very convenient for type inference.  Thus, we enforce disjointness to keep things simple.<br>
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

(* The examples from the last section may seem cute but not especially useful.  In this section, we meet <tt>map</tt>, the real workhorse of Ur's type-level computation.  We'll use it to type some useful operations over value-level records.  A few more pieces will be necessary before getting there, so we'll start just by showing how interesting type-level operations on records may be built from <tt>map</tt>. *)

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


(* * Folders *)

(* We're almost ready to implement some more polymorphic operations on records.  The key missing piece is <b>folders</b>; more specifically, the type family <tt>folder</tt> that allows iteration over the fields of type-level records.  The Ur standard library exposes <tt>folder</tt> abstractly, along with the following key operation over it.  Don't mind the clutter at the end of this definition, where we rebind the function <tt>fold</tt> from the default-open module <tt>Top</tt>, as we must include an explicit kind-polymorphic binder to appease the associated let-polymorphism.  (A normal program would omit this definition, anyway; we include it here only to show the type of <tt>fold</tt>.) *)

val fold : K --> tf :: ({K} -> Type)
           -> (nm :: Name -> v :: K -> r :: {K} -> [[nm] ~ r] =>
               tf r -> tf ([nm = v] ++ r))
           -> tf []
           -> r ::: {K} -> folder r -> tf r
         = K ==> fold

(* The type is a bit of a mouthful.  We can describe the function arguments in order.  First, <tt>K</tt> is the kind of data associated with fields in the record we will be iterating over.  Next, <tt>tf</tt> describes the type of an accumulator, much as for standard "fold" operations over lists.  The difference here is that the accumulator description is not a mere type, but rather a <b>type-level function</b> that returns a type given a properly kinded record.  When we begin iterating over a record <tt>r</tt>, the accumulator has type <tt>tf []</tt> (where <tt>[]</tt> is the empty record), and when we finish iterating, the accumulator has type <tt>tf r</tt>.  As we step through the fields of the record, we add each one to the argument we keep passing to <tt>tf</tt>.<br>
<br>
The next arguments of <tt>fold</tt> are much like for normal list fold functions: a step function and an initial value.  The latter has type <tt>tf []</tt>, just as we expect from the explanation in the last paragraph.  The final arguments are <tt>r</tt>, the record we fold over; and a <tt>folder</tt> for it.  The function return type follows last paragraph's explanation of accmulator typing.<br>
<br>
We've left a big unexplained piece: the type of the step function.  In order, its arguments are <tt>nm</tt>, the current field being processed; <tt>v</tt>, the data associated with that field; <tt>r</tt>, the portion of the input record that we had already stepped through before this point; a proof that the name <tt>nm</tt> didn't already occur in <tt>r</tt>; and the accumulator, typed to show that the set of fields we've already visited is exactly <tt>r</tt>.  The return type of the step function is another accumulator type, extended to show that now we've visited <tt>nm</tt>, too.<br>
<br>
Here's a simple example usage, where we write a function to count the number of fields in a type-level record of types. *)

fun countFields [ts :: {Type}] (fl : folder ts) : int =
    @fold [fn _ => int] (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] n => n + 1) 0 fl

(* We preface <tt>fold</tt> with <tt>@</tt>, to disable inference of folders, since we have one we'd like to pass explicitly.  The accumulator type family we use is a simple one that ignores its argument and always returns <tt>int</tt>; at every stage of our traversal of input record <tt>ts</tt>, we keep an integer as our sole state, and the type of this state doesn't depend on which record fields we've visited.  The step function binds each type parameter with the notation <tt>::_</tt>, for an explicit parameter whose kind should be inferred.<br>
<br>
The function <tt>countFields</tt> is a lot easier to use than it is to define!  Here's an example invocation, where we see that the appropriate folder is inferred. *)

(* begin eval *)
countFields [[A = int, B = float, C = string]]
(* end *)

(* If folders are generally inferred, why bother requiring that they be passed around?  The answer has to do with Ur's rule that type-level records are considered equivalent modulo permutation.  As a result, there is no unique traversal order for a record, in general.  The programmer has freedom in constructing folders that embody different permutations, using the functions exposed from the module <tt>Folder</tt> (see the top of <tt>lib/ur/top.urs</tt> in the Ur/Web distribution).  Still, in most cases, the order in which fields are written in the source code provides an unambiguous clue about desired ordering.  Thus, by default, folder parameters are implicit, and they are inferred to follow the order of fields in program text.<br>
<br>
Let's implement a more ambitious traversal.  We will take in a record whose fields all contain <tt>option</tt> types, and we will determine if every field contains a <tt>Some</tt>.  If so, we return <tt>Some</tt> of a "de-optioned" version; if not, we return <tt>None</tt>. *)

fun join [ts ::: {Type}] (fl : folder ts) (r : $(map option ts)) : option $ts =
    @fold [fn ts => $(map option ts) -> option $ts]
     (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] (f : $(map option r) -> option $r) =>
         fn r : $(map option ([nm = v] ++ r)) =>
            case r.nm of
                None => None
              | Some v =>
                case f (r -- nm) of
                    None => None
                  | Some vs => Some ({nm = v} ++ vs))
     (fn _ : $(map option []) => Some {}) fl r

(* Rather than take in an arbitrary record type and add some sort of constraint requiring that it contain only <tt>option</tt> types, the Ur way is to <b>construct</b> a record type with computation over some more primitive inputs, such that the process (A) is guaranteed to construct only records satisfying the constraint and (B) is capable, given the proper inputs, of constructing any record satisfying the original constraint.<br>
<br>
Our use of folding here involves an accumulator type that <i>is</i> record-dependent.  In particular, as we traverse the record, we are building up a "de-optioning" function.  To implement the step function, we rely on the record projection form <tt>r.nm</tt> and the record field removal form <tt>r -- nm</tt>, both of which work fine with variables standing for unknown field names.  To extend the output record with a new mapping for field <tt>nm</tt>, we use concatenation <tt>++</tt> with a singleton record literal.<br>
<br>
Like for the last example, <tt>join</tt> is much easier to use than to implement!  The simple invocations below use Ur's <b>reverse-engineering unification</b> to deduce the value of parameter <tt>ts</tt> from the type of parameter <tt>r</tt>.  Also, as before, the folder argument is inferred. *)

(* begin hide *)
fun show_option [t] (_ : show t) : show (option t) =
    mkShow (fn x => case x of
                        None => "None"
                      | Some x => "Some(" ^ show x ^ ")")

val show_r = mkShow (fn r : {A : int, B : string} =>
                        "{A = " ^ show r.A ^ ", B = " ^ show r.B ^ "}")
(* end *)

(* begin eval *)
join {A = Some 1, B = Some "X"}
(* end *)

(* begin eval *)
join {A = Some 1, B = None : option string}
(* end *)

(* The Ur/Web standard library includes many variations on <tt>fold</tt> that encapsulate common traversal patterns.  For instance, <tt>foldR</tt> captures the idea of folding over a value-level record, and we can use it to simplify the definition of <tt>join</tt>: *)

fun join [ts ::: {Type}] (fl : folder ts) (r : $(map option ts)) : option $ts =
    @foldR [option] [fn ts => option $ts]
     (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] v vs =>
         case (v, vs) of
             (Some v, Some vs) => Some ({nm = v} ++ vs)
           | _ => None)
     (Some {}) fl r

(* See <tt>lib/ur/top.urs</tt> for the types of <tt>foldR</tt> and some other handy folding functions. *)


(* * Working with First-Class Disjointness Obligations *)

(* The syntax <tt>[r1 ~ r2]</tt> in a function definition introduces a constraint that the type-level records <tt>r1</tt> and <tt>r2</tt> share no field names.  The same syntax may also be used with anonymous function definitions, which can be useful in certain kinds of record traversals, as we've seen in the types of step functions above.  Sometimes we must mark explicitly the places where these disjointness proof obligations must be proved.  To pick a simple example, let's pretend that the general value-level <tt>++</tt> operator is missing form the language, so that we must implement it ourselves on top of a version of <tt>++</tt> that can only add one field at a time.  The following code demonstrates the use of the syntax <tt>!</tt> to discharge a disjointness obligation explicitly.  We generally only need to do that when working with the <tt>@</tt> version of an identifier, which not only requires that folders be passed explicitly, but also disjointness proofs (which are always written as just <tt>!</tt>) and type class witnesses. *)

fun concat [ts1 ::: {Type}] [ts2 ::: {Type}] [ts1 ~ ts2]
           (fl : folder ts1) (r1 : $ts1) (r2 : $ts2) : $(ts1 ++ ts2) =
    @foldR [id] [fn ts1 => ts2 ::: {Type} -> [ts1 ~ ts2] => $ts2 -> $(ts1 ++ ts2)]
     (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] v
                  (acc : ts2 ::: {Type} -> [r ~ ts2] => $ts2 -> $(r ++ ts2))
                  [ts2] [[nm = v] ++ r ~ ts2] r =>
         {nm = v} ++ acc r)
     (fn [ts2] [[] ~ ts2] (r : $ts2) => r) fl r1 ! r2

(* begin hide *)
val show_r = mkShow (fn r : {A : int, B : string, C : float, D : bool} =>
                        "{A = " ^ show r.A ^ ", B = " ^ show r.B ^ ", C = " ^ show r.C ^ ", D = " ^ show r.D ^ "}")
(* end *)

(* begin eval *)
concat {A = 1, B = "x"} {C = 2.3, D = True}
(* end *)


(* * Type-Level Computation Meets Type Classes *)

(* Ur's treatment of type classes makes some special allowances for records.  In particular, a record of type class witnesses may be inferred automatically.  Our next example shows how to put that functionality to good use, in writing a function for pretty-printing records as strings.  The type class <tt>show</tt> is part of the Ur standard library, and its instances are valid arguments to the string-producing function <tt>show</tt>. *)

fun showRecord [ts ::: {Type}] (fl : folder ts) (shows : $(map show ts))
               (names : $(map (fn _ => string) ts)) (r : $ts) : string =
    "{" ^ @foldR3 [fn _ => string] [show] [id] [fn _ => string]
           (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] name shower value acc =>
               name ^ " = " ^ @show shower value ^ ", " ^ acc)
           "...}" fl names shows r

(* begin eval *)
showRecord {A = "A", B = "B"} {A = 1, B = 2.3}
(* end *)

(* One natural complaint about this code is that field names are repeated unnecessarily as strings.  Following Ur's design rationale, this is a consequence of a "feature," not a "bug," since allowing the syntactic analysis of type-level data would break the celebrated property of <b>parametricity</b>. *)


(* * Type-Level Computation Meets Modules *)

(* To illustrate how the features from this chapter integrate with Ur's module system, let's reimplement the last example as a functor. *)

functor ShowRecord(M : sig
                       con ts :: {Type}
                       val fl : folder ts
                       val shows : $(map show ts)
                       val names : $(map (fn _ => string) ts)
                   end) : sig
                            val show_ts : show $M.ts
                          end = struct
    open M

    val show_ts = mkShow (fn r : $ts =>
        "{" ^ @foldR3 [fn _ => string] [show] [id] [fn _ => string]
               (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] name shower value acc =>
                   name ^ " = " ^ @show shower value ^ ", " ^ acc)
               "...}" fl names shows r)
end

open ShowRecord(struct
                    con ts = [A = int, B = float, C = bool]
                    val names = {A = "A", B = "B", C = "C"}
                end)

(* begin eval *)
show {A = 1, B = 2.3, C = True}
(* end *)

(* A few important points show up in this example.  First, Ur extends the standard ML module system feature base by allowing functors to be applied to structures with omitted members, when those members can be inferred from the others.  Thus, we call <tt>ShowRecord</tt> omitting the fields <tt>fl</tt> and <tt>shows</tt>.  Second, merely calling a functor that produces an output with a type class instance can bring that instance into scope, so that it is applied automatically, as in our evaluation example above.<br>
<br>
To illustrate the mixing of constraints and functors, we translate another of our earlier examples in a somewhat silly way: *)

functor Concat(M : sig
                   con f :: Type -> Type
                   con r1 :: {Type}
                   con r2 :: {Type}
                   constraint r1 ~ r2
               end) : sig
                        val concat : $(map M.f M.r1) -> $(map M.f M.r2) -> $(map M.f (M.r1 ++ M.r2))
                      end = struct
    fun concat r1 r2 = r1 ++ r2
end

structure C = Concat(struct
                         con f = id
                         con r1 = [A = int]
                         con r2 = [B = float, C = bool]
                     end)

(* begin eval *)
show (C.concat {A = 6} {B = 6.0, C = False})
(* end *)
