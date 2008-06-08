con c1 = int
con c2 = (fn t :: Type => t) int

con id = fn t :: Type => t
con c3 = id int

con fst = fn t1 :: Type => fn t2 :: Type => t1
con c4 = fst int string

con snd = fn t1 :: Type => fn t2 :: Type => t2
con c5 = snd int string

con apply = fn f :: Type -> Type => fn t :: Type => f t
con c6 = apply id int
con c7 = apply (fst int) string

val tickle = fn n :: Name => fn t :: Type => fn fs :: {Type} =>
        fn x : $([n = t] ++ fs) => x
val tickleA = tickle[#A][int][[B = string]]
val test_tickleA = tickleA {A = 6, B = "13"}

val grab = fn n :: Name => fn t ::: Type => fn fs ::: {Type} =>
        fn x : $([n = t] ++ fs) => x.n
val test_grab1 = grab[#A] {A = 6, B = "13"}
val test_grab2 = grab[#B] {A = 6, B = "13"}

val main = {A = test_grab1, B = test_grab2}
