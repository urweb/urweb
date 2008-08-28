datatype list a = Nil | Cons of a * list a

fun append (t ::: Type) (ls1 : list t) (ls2 : list t) : list t =
        case ls1 of
            Nil => ls2
          | Cons (h, t) => Cons (h, append t ls2)

(*val rec ones : list int = Cons (1, ones)*)
val rec ones = fn () => Cons (1, ones ())
