datatype list a = Nil | Cons of a * list a

fun isNil (t ::: Type) (ls : list t) : bool =
        case ls of
          Nil => True
        | Cons _ => False

(*fun append (t ::: Type) (ls1 : list t) (ls2 : list t) : list t =
        case ls1 of
          Nil => ls2
        | Cons (x, ls1') => Cons (x, append ls1' ls2)

fun delist (ls : list string) : xml body [] [] =
        case ls of
          Nil => <body>Nil</body>
        | Cons (h, t) => <body>{cdata h} :: {delist t}</body>*)

val ls = Cons ("X", Cons ("Y", Cons ("Z", Nil)))

fun main () : transaction page = return <html><body>
        {if isNil ls then <body>It's Nil.</body> else <body>It's not Nil.</body>}
</body></html>


(*        <p>{delist ls}</p>*)

