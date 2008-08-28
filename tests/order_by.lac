table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int}

val q1 = (SELECT * FROM t1 ORDER BY t1.A, t1.B)
val q2 = (SELECT * FROM t1 GROUP BY t1.A ORDER BY t1.A, t1.B)
val q3 = (SELECT t1.B FROM t1
        UNION SELECT t1.B FROM t1
        ORDER BY t1.B)

val q4 = (SELECT t1.A, t2.D, t1.A < t2.D AS Lt
        FROM t1, t2
        ORDER BY Lt)
val q5 = (SELECT t1.A, t2.D, t1.A < t2.D AS Lt
        FROM t1, t2
        ORDER BY t1.A, Lt, t2.D)
