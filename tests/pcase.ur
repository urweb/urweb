val flip = fn x : int => case x of 0 => 1 | _ => 0

val zero = flip 1
val one = flip 0

val flipS = fn x : string => case x of "" => "Hello world!" | _ => ""

val s1 = flipS ""
val s2 = flipS "Boop"
