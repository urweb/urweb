datatype list t = Nil | Cons of t * list t

fun length [t] (ls : list t) =
    let
        fun length' (ls : list t) (acc : int) =
            case ls of
                Nil => acc
              | Cons (_, ls') => length' ls' (acc + 1)
    in
        length' ls 0
    end

fun rev [t] (ls : list t) = 
    let
        fun rev' (ls : list t) (acc : list t) =
            case ls of
                Nil => acc
              | Cons (x, ls') => rev' ls' (Cons (x, acc))
    in
        rev' ls Nil
    end
