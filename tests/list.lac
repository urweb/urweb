datatype list a = Nil | Cons of a * list a

val isNil = fn t ::: Type => fn ls : list t =>
        case ls of Nil => True | _ => False

val show = fn b => if b then "True" else "False"

val rec delist : list string -> xml body [] [] = fn x =>
        case x of
          Nil => <body>Nil</body>
        | Cons (h, t) => <body>{cdata h} :: {delist t}</body>

val main : unit -> page = fn () => <html><body>
        {cdata (show (isNil (Nil : list bool)))},
        {cdata (show (isNil (Cons (1, Nil))))},
        {cdata (show (isNil (Cons ("A", Cons ("B", Nil)))))}

        <p>{delist (Cons ("X", Cons ("Y", Cons ("Z", Nil))))}</p>
</body></html>
