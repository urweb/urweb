datatype list a = Nil | Cons of a * list a

fun isNil (t ::: Type) (ls : list t) : bool =
        case ls of
          Nil => True
        | Cons _ => False

fun append (t ::: Type) (ls1 : list t) (ls2 : list t) : list t =
        case ls1 of
          Nil => ls2
        | Cons (x, ls1') => Cons (x, append ls1' ls2)

fun appendR (t ::: Type) (ls2 : list t) (ls1 : list t) : list t =
        case ls1 of
          Nil => ls2
        | Cons (x, ls1') => Cons (x, appendR ls2 ls1')

(*fun naughty (t ::: Type) (ls : list t) : list t = naughty ls*)

fun append1 (t ::: Type) (ls1 : list t) (ls2 : list t) : list t =
        case ls1 of
          Nil => ls2
        | Cons (x, ls1') => Cons (x, append2 ls2 ls1')

and append2 (t ::: Type) (ls2 : list t) (ls1 : list t) : list t =
        case ls1 of
          Nil => ls2
        | Cons (x, ls1') => Cons (x, append1 ls1' ls2)
