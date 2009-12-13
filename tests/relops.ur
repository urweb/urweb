table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int}

val q1 = (SELECT * FROM t1
        UNION SELECT * FROM t1)
val q2 = (SELECT t1.A, t1.B FROM t1 WHERE t1.A = 0
        INTERSECT SELECT t1.B, t1.A FROM t1 WHERE t1.B = t1.B)
val q3 = (SELECT t1.A, t1.B, t1.C FROM t1 WHERE t1.A = 0
        INTERSECT SELECT * FROM t1 WHERE t1.B = 'Hello world!'
        EXCEPT SELECT * FROM t1 WHERE t1.A < t1.A
        UNION SELECT * FROM t1 WHERE t1.A > t1.A)

datatype list a = Nil | Cons of a * list a

val r1 : transaction (list {A : int, B : string, C : float}) =
        query q3
        (fn fs acc => return (Cons (fs.T1, acc)))
        Nil

val r2 : transaction string =
        ls <- r1;
        return (case ls of
                    Nil => "Problem"
                  | Cons ({B = b, ...}, _) => b)

val main : unit -> transaction page = fn () =>
        s <- r2;
        return <xml><body>
                {cdata s}
        </body></xml>
