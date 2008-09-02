table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int}

val q1 = (SELECT * FROM t1)
val q2 = (SELECT * FROM t1 WHERE TRUE)
val q3 = (SELECT * FROM t1 WHERE FALSE)
val q4 = (SELECT * FROM t1 WHERE {True})
val q5 = (SELECT * FROM t1 WHERE {1} = {1})
val q6 = (SELECT * FROM t1 WHERE {"Hi"} < {"Bye"})
val q7 = (SELECT * FROM t1 WHERE {1} = {1} AND NOT ({"Hi"} <= {"Bye"}))
val q8 = (SELECT * FROM t1 WHERE t1.A = 1 OR t1.C < 3.0)

datatype list a = Nil | Cons of a * list a

val r1 : transaction (list {A : int, B : string, C : float}) =
        query q7
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
