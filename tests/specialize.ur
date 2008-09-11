datatype list a = Nil | Cons of a * list a

fun isNil (t ::: Type) (ls : list t) : bool =
        case ls of
          Nil => True
        | Cons _ => False

fun append (t ::: Type) (ls1 : list t) (ls2 : list t) : list t =
        case ls1 of
          Nil => ls2
        | Cons (x, ls1') => Cons (x, append ls1' ls2)

fun pairAppend (t1 ::: Type) (t2 ::: Type) (ls1 : list (t1 * t2)) (ls2 : list (t1 * t2)) : list (t1 * t2) =
        case ls1 of
          Nil => ls2
        | Cons (x, ls1') => Cons (x, pairAppend ls1' ls2)

fun delist (ls : list string) : xml body [] [] =
        case ls of
          Nil => <body>Nil</body>
        | Cons (h, t) => <body>{cdata h} :: {delist t}</body>

fun pairDelist (ls : list (string * int)) : xml body [] [] =
        case ls of
          Nil => <body>Nil</body>
        | Cons ((s, n), t) => <body>({cdata s}, {cdata (show _ n)}) :: {pairDelist t}</body>

val ls = Cons ("X", Cons ("Y", Cons ("Z", Nil)))
val ls' = Cons ("A", Cons ("B", Nil))

val pls = Cons (("X", 1), Cons (("Y", 2), Cons (("Z", 3), Nil)))
val pls' = Cons (("A", 1), Cons (("B", 2), Nil))

fun main () : transaction page = return <html><body>
        {if isNil ls then <body>It's Nil.</body> else <body>It's not Nil.</body>}

        <p>{delist (append ls' ls)}</p>

        <p>{pairDelist (pairAppend pls' pls)}</p>
</body></html>
