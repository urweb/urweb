table t : {A : int, B : string, C : float}

val q1 = (SELECT * FROM t LIMIT 42)
val q2 = fn n => (SELECT * FROM t LIMIT {n})

val q3 = (SELECT * FROM t OFFSET 3)
val q4 = fn n => fn m => (SELECT * FROM t LIMIT {n} OFFSET {m})


datatype list a = Nil | Cons of a * list a

val r1 : transaction (list {A : int, B : string, C : float}) =
        query (q4 3 7)
        (fn fs acc => return (Cons (fs.T, acc)))
        Nil

val r2 : transaction string =
        ls <- r1;
        return (case ls of
                    Nil => "Problem"
                  | Cons ({B = b, ...}, _) => b)

val main : unit -> transaction page = fn () =>
        s <- r2;
        return <html><body>
                {cdata s}
        </body></html>
