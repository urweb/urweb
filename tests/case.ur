datatype t = A | B

val swap = fn x : t => case x of A => B | B => A

datatype u = C of t | D

val out = fn x : u => case x of C y => y | D => A

datatype nat = O | S of nat

val is_two = fn x : nat =>
        case x of S (S O) => A | _ => B

val shw = fn x : t =>
             case x of A => "A" | B => "B"

fun main (): transaction page = return <xml><body>
  <div>
    <p>zero is two: {[shw (is_two O)]}</p>
    <p>one is two: {[shw (is_two (S O))]}</p>
    <p>two is two: {[shw (is_two (S (S O)))]}</p>
  </div>

  <button onclick={fn _ => let
                     val m =
                         "zero is two: " ^ shw (is_two O) ^ "\n" ^
                         "one is two: " ^ shw (is_two (S O)) ^ "\n" ^
                         "two is two: " ^ shw (is_two (S (S O)))
                 in
                     alert m
                 end}>click me</button>
</body></xml>
