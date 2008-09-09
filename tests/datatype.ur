datatype t = A | B

val a = A
val b = B

datatype foo = C of t

val c = C a

datatype list = Nil | Cons of {Head : int, Tail : list}

val nil = Nil
val l1 = Cons {Head = 0, Tail = nil}

datatype term = App of term * term | Abs of term -> term

