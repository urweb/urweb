table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int}

val q1 = (SELECT * FROM t1
        UNION SELECT * FROM t1)
val q2 = (SELECT t1.A, t1.B FROM t1 WHERE t1.A = 0
        INTERSECT SELECT t1.B, t1.A FROM t1 WHERE t1.B = t1.B)
val q3 = (SELECT t1.A, t1.B, t1.C FROM t1 WHERE t1.A = 0
        INTERSECT SELECT * FROM t1 WHERE t1.B = 'Hello world!'
        EXCEPT SELECT * FROM t1 WHERE t1.A < t1.A)
