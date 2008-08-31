table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int}

val q1 = (SELECT * FROM t1 GROUP BY t1.B)
val q2 = (SELECT * FROM t1, t2 GROUP BY t1.B, t2.D, t1.A)

val q3 = (SELECT * FROM t1 WHERE t1.A = 0 GROUP BY t1.B)
val q4 = (SELECT * FROM t1 WHERE t1.A = 0 GROUP BY t1.B HAVING t1.B <> 'Bad')

val q5 = (SELECT t1.A, t2.D FROM t1, t2 GROUP BY t2.D, t1.A)
val q6 = (SELECT t1.A, t2.D FROM t1, t2 WHERE t1.C = 0.0 GROUP BY t2.D, t1.A HAVING t1.A = t1.A AND t2.D = 17)


datatype list a = Nil | Cons of a * list a

val r1 : transaction (list {B : string}) =
        query q4
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
