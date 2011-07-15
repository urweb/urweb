(* Introduction *)

(* begin hide *)
val show_string = mkShow (fn s => "\"" ^ s ^ "\"")
(* end *)

(* This tutorial by <a href="http://adam.chlipala.net/">Adam Chlipala</a> is licensed under a <a href="http://creativecommons.org/licenses/by-nc-nd/3.0/">Creative Commons Attribution-Noncommercial-No Derivative Works 3.0 Unported License</a>. *)

(* This is a tutorial for the <a href="http://www.impredicative.com/ur/">Ur/Web</a> programming language.  The <a href="http://www.impredicative.com/ur/">official project web site</a> is your starting point for information, like a reference manual and where to download the latest code release.  In this tutorial, we'll just focus on introducing the language features. *)

(* Ur/Web contains a web-indendent core language called Ur, which will be the subject of the first few chapters of the tutorial.  Ur inherits its foundation from ML and Haskell, then going further to add fancier stuff.  This first chapter of the tutorial reviews the key ML and Haskell features, giving their syntax in Ur. *)

(* * Basics *)

(* Let's start with features shared with both ML and Haskell.  First, we have the basic numeric, string, and Boolean stuff.  (In the following examples, "==" is used to indicate the result of evaluating an expression.  It's not valid Ur syntax!) *)

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

(* The "option" type family is like ML's "option" or Haskell's "maybe."  We also have a "case" expression form lifted directly from ML.  Note that, while Ur follows most syntactic conventions of ML, one key difference is that type families appear before their arguments, as in Haskell. *)

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
