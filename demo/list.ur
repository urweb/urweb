datatype list t = Nil | Cons of t * list t

fun length' (t ::: Type) (ls : list t) (acc : int) =
    case ls of
        Nil => acc
      | Cons (_, ls') => length' ls' (acc + 1)

fun length (t ::: Type) (ls : list t) = length' ls 0

fun rev' (t ::: Type) (ls : list t) (acc : list t) =
    case ls of
        Nil => acc
      | Cons (x, ls') => rev' ls' (Cons (x, acc))

fun rev (t ::: Type) (ls : list t) = rev' ls Nil
