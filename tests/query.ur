table t1 : {A : int, B : string, C : float, D : bool}
table t2 : {A : float, D : int}

datatype list a = Nil | Cons of a * list a

(*val q1 = (SELECT * FROM t1)*)
val q1 = (SELECT * FROM t1 WHERE t1.A = 0)

val r1 : transaction (list {A : int, B : string, C : float, D : bool}) =
        query q1
        (fn fs acc => return (Cons (fs.T1, acc)))
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
