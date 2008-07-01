con c1 = fn x :: Name => [x] ~ [A] => [x = int, A = string]
con c2 = fn x :: Name => [x] ~ [A] => [A, x]
con c3 = fn x :: Name => [A] ~ [x] => [x, A]
con c4 = fn x :: Name => [A] ~ [x] => [A, x]

con c5 = fn r1 :: {Type} => fn r2 => r1 ~ r2 => r1 ++ r2
con c6 = fn r1 :: {Type} => fn r2 => r2 ~ r1 => r1 ++ r2

con c7 = fn x :: Name => fn r => [x] ~ r => [x = int] ++ r

val vt1 = fn x : $(c1 #B) => x.B
val vt2 = fn x : $(c1 #B) => x.A
val vt3 = fn x : $(c1 #C) => x.A
val vt4 = fn x : $(c1 #C) => x.A
(*
val vtX = fn x : $(c1 #A) => x.A
val vtX = fn x : $(c1 #A) => x.A
*)

val v1 = fn x :: Name => fn [x] ~ [A] => fn y : {x : int, A : string} => y.x

val vt5 = v1 [#B] {A = "Hi", B = 0}
(*
val vtX = v1 [#A] {A = "Hi", A = 0}
*)

val v2 = fn x :: Name => fn r :: {Type} => fn y : $(c7 x r) => fn [x] ~ r => y.x
val vt6 = v2 [#A] [[B = float, C = string]] {A = 8, B = 8.0, C = "8"}

(*
val vtX = v2 [#A] [[B = float, B = string]] {A = 8, B = 8.0, B = "8"}
val vtX = v2 [#A] [[A = float, B = string]] {A = 8, A = 8.0, B = "8"}
*)

val main = vt6
