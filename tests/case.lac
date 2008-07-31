datatype t = A | B

val swap = fn x : t => case x of A => B | B => A

datatype u = C of t | D

val out = fn x : u => case x of C y => y | D => A

datatype nat = O | S of nat

val is_two = fn x : nat =>
        case x of S (S O) => A | _ => B

val zero_is_two = is_two O
val one_is_two = is_two (S O)
val two_is_two = is_two (S (S O))
