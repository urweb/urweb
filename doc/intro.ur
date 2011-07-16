(* Chapter 1: Introduction *)

(* begin hide *)
val show_string = mkShow (fn s => "\"" ^ s ^ "\"")
(* end *)

(* This tutorial by <a href="http://adam.chlipala.net/">Adam Chlipala</a> is licensed under a <a href="http://creativecommons.org/licenses/by-nc-nd/3.0/">Creative Commons Attribution-Noncommercial-No Derivative Works 3.0 Unported License</a>. *)

(* This is a tutorial for the <a href="http://www.impredicative.com/ur/">Ur/Web</a> programming language.  The <a href="http://www.impredicative.com/ur/">official project web site</a> is your starting point for information, like a reference manual and where to download the latest code release.  In this tutorial, we'll just focus on introducing the language features. *)

(* Ur/Web contains a web-indendent core language called Ur, which will be the subject of the first few chapters of the tutorial.  Ur inherits its foundation from ML and Haskell, then going further to add fancier stuff.  This first chapter of the tutorial reviews the key ML and Haskell features, giving their syntax in Ur. *)

(* * Basics *)

(* Let's start with features shared with both ML and Haskell.  First, we have the basic numeric, string, and Boolean stuff.  (In the following examples, <tt>==</tt> is used to indicate the result of evaluating an expression.  It's not valid Ur syntax!) *)

(* begin eval *)
1 + 1
(* end *)

(* begin eval *)
1.2 + 3.4
(* end *)

(* begin eval *)
"Hello " ^ "world!"
(* end *)

(* begin eval *)
1 + 1 < 6
(* end *)

(* begin eval *)
0.0 < -3.2
(* end *)

(* begin eval *)
"Hello" = "Goodbye" || (1 * 2 <> 8 && True <> False)
(* end *)

(* We also have function definitions with type inference for parameter and return types. *)

fun double n = 2 * n

(* begin eval *)
double 8
(* end *)

fun fact n = if n = 0 then 1 else n * fact (n - 1)

(* begin eval *)
fact 5
(* end *)

(* Of course we have anonymous functions, too. *)

val inc = fn x => x + 1

(* begin eval *)
inc 3
(* end *)

(* Then there's parametric polymorphism.  Unlike in ML and Haskell, polymorphic functions in Ur/Web often require full type annotations.  That is because more advanced features (which we'll get to in the next chapter) make Ur type inference undecidable. *)

fun id [a] (x : a) : a = x

(* begin eval *)
id "hi"
(* end *)

fun compose [a] [b] [c] (f : b -> c) (g : a -> b) (x : a) : c = f (g x)

(* begin eval *)
compose inc inc 3
(* end *)

(* The <tt>option</tt> type family is like ML's <tt>option</tt> or Haskell's <tt>Maybe</tt>.  We also have a <tt>case</tt> expression form lifted directly from ML.  Note that, while Ur follows most syntactic conventions of ML, one key difference is that type families appear before their arguments, as in Haskell. *)

fun predecessor (n : int) : option int = if n >= 1 then Some (n - 1) else None

(* begin hide *)
fun show_option [t] (_ : show t) : show (option t) =
    mkShow (fn x => case x of
                        None => "None"
                      | Some x => "Some(" ^ show x ^ ")")
(* end *)

(* begin eval *)
predecessor 6
(* end *)

(* begin eval *)
predecessor 0
(* end *)

(* Naturally, there are lists, too! *)

val numbers : list int = 1 :: 2 :: 3 :: []
val strings : list string = "a" :: "bc" :: []

fun length [a] (ls : list a) : int =
    case ls of
        [] => 0
      | _ :: ls' => 1 + length ls'

(* begin eval *)
length numbers
(* end *)

(* begin eval *)
length strings
(* end *)

(* And lists make a good setting for demonstrating higher-order functions and local functions.  (This example also introduces one idiosyncrasy of Ur, which is that <tt>map</tt> is a keyword, so we name our"map" function <tt>mp</tt>. *)

(* begin hide *)
fun show_list [t] (_ : show t) : show (list t) =
    mkShow (let
                fun shower (ls : list t) =
                    case ls of
                        [] => "[]"
                      | x :: ls' => show x ^ " :: " ^ shower ls'
            in
                shower
            end)
(* end *)

fun mp [a] [b] (f : a -> b) : list a -> list b =
    let
        fun loop (ls : list a) =
            case ls of
                [] => []
              | x :: ls' => f x :: loop ls'
    in
        loop
    end

(* begin eval *)
mp inc numbers
(* end *)

(* begin eval *)
mp (fn s => s ^ "!") strings
(* end *)

(* We can define our own polymorphic datatypes and write higher-order functions over them. *)

datatype tree a = Leaf of a | Node of tree a * tree a

(* begin hide *)
fun show_tree [t] (_ : show t) : show (tree t) =
    mkShow (let
                fun shower (t : tree t) =
                    case t of
                        Leaf x => "Leaf(" ^ show x ^ ")"
                      | Node (t1, t2) => "Node(" ^ shower t1 ^ ", " ^ shower t2 ^ ")"
            in
                shower
            end)
(* end *)

fun size [a] (t : tree a) : int =
    case t of
        Leaf _ => 1
      | Node (t1, t2) => size t1 + size t2

(* begin eval *)
size (Node (Leaf 0, Leaf 1))
(* end *)

(* begin eval *)
size (Node (Leaf 1.2, Node (Leaf 3.4, Leaf 4.5)))
(* end *)

fun tmap [a] [b] (f : a -> b) : tree a -> tree b =
    let
        fun loop (t : tree a) : tree b =
            case t of
                Leaf x => Leaf (f x)
              | Node (t1, t2) => Node (loop t1, loop t2)
    in
        loop
    end

(* begin eval *)
tmap inc (Node (Leaf 0, Leaf 1))
(* end *)

(* We also have anonymous record types, as in Standard ML.  The next chapter will show that there is quite a lot more going on here with records than in SML or OCaml, but we'll stick to the basics in this chapter.  We will add one tantalizing hint of what's to come by demonstrating the record concatention operator <tt>++</tt> and the record field removal operator <tt>--</tt>. *)

val x = { A = 0, B = 1.2, C = "hi", D = True }

(* begin eval *)
x.A
(* end *)

(* begin eval *)
x.C
(* end *)

type myRecord = { A : int, B : float, C : string, D : bool }

fun getA (r : myRecord) = r.A

(* begin eval *)
getA x
(* end *)

(* begin eval *)
getA (x -- #A ++ {A = 4})
(* end *)

val y = { A = "uhoh", B = 2.3, C = "bye", D = False }

(* begin eval *)
getA (y -- #A ++ {A = 5})
(* end *)


(* * Borrowed from ML *)

(* Ur includes an ML-style module system.  The most basic use case involves packaging abstract types with their "methods." *)

signature COUNTER = sig
    type t
    val zero : t
    val increment : t -> t
    val toInt : t -> int
end

structure Counter : COUNTER = struct
    type t = int
    val zero = 9
    val increment = plus 1
    fun toInt x = x
end

(* begin eval *)
Counter.toInt (Counter.increment Counter.zero)
(* end *)

(* We may package not just abstract types, but also abstract type families.  Here we see our first use of the <tt>con</tt> keyword, which stands for <b>constructor</b>.  Constructors are a generalization of types to include other "compile-time things"; for instance, type families, which are assigned the kind <tt>Type -> Type</tt>.  Kinds are to constructors as types are to normal values.  We also see how to write the type of a polymorphic function, using the <tt>:::</tt> syntax for type variable binding.  This <tt>:::</tt> differs from the <tt>::</tt> used with the <tt>con</tt> keyword because it marks a type parameter as implicit, so that it need not be supplied explicitly at call sites.  Such an option is the only one available in ML and Haskell, but, in the next chapter, we'll meet cases where it is appropriate to use explicit constructor parameters. *)

signature STACK = sig
    con t :: Type -> Type
    val empty : a ::: Type -> t a
    val push : a ::: Type -> t a -> a -> t a
    val pop : a ::: Type -> t a -> option a
end

structure Stack : STACK = struct
    con t = list
    val empty [a] = []
    fun push [a] (t : t a) (x : a) = x :: t
    fun pop [a] (t : t a) = case t of
                                [] => None
                              | x :: _ => Some x
end

(* begin eval *)
Stack.pop (Stack.push (Stack.push Stack.empty "A") "B")
(* end *)

(* Ur also inherits the ML concept of <b>functors</b>, which are functions from modules to modules. *)

datatype order = Less | Equal | Greater

signature COMPARABLE = sig
    type t
    val compare : t -> t -> order
end

signature DICTIONARY = sig
    type key
    con t :: Type -> Type
    val empty : a ::: Type -> t a
    val insert : a ::: Type -> t a -> key -> a -> t a
    val lookup : a ::: Type -> t a -> key -> option a
end

functor BinarySearchTree(M : COMPARABLE) : DICTIONARY where type key = M.t = struct
    type key = M.t
    datatype t a = Leaf | Node of t a * key * a * t a

    val empty [a] = Leaf

    fun insert [a] (t : t a) (k : key) (v : a) : t a =
        case t of
            Leaf => Node (Leaf, k, v, Leaf)
          | Node (left, k', v', right) =>
            case M.compare k' k of
                Equal => Node (left, k, v, right)
              | Less => Node (insert left k v, k', v', right)
              | Greater => Node (left, k', v', insert right k v)

    fun lookup [a] (t : t a) (k : key) : option a =
        case t of
            Leaf => None
          | Node (left, k', v, right) =>
            case M.compare k' k of
                Equal => Some v
              | Less => lookup left k
              | Greater => lookup right k
end

structure IntTree = BinarySearchTree(struct
                                         type t = int
                                         fun compare n1 n2 =
                                             if n1 = n2 then
                                                 Equal
                                             else if n1 < n2 then
                                                 Less
                                             else
                                                 Greater
                                     end)

(* begin eval *)
IntTree.lookup (IntTree.insert (IntTree.insert IntTree.empty 0 "A") 1 "B") 1
(* end *)

(* It is sometimes handy to rebind modules to shorter names. *)

structure IT = IntTree

(* begin eval *)
IT.lookup (IT.insert (IT.insert IT.empty 0 "A") 1 "B") 0
(* end *)

(* One can even use the <tt>open</tt> command to import a module's namespace wholesale, though this can make it harder for someone reading code to tell which identifiers come from which modules. *)

open IT

(* begin eval *)
lookup (insert (insert empty 0 "A") 1 "B") 2
(* end *)
