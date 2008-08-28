table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int}

val q1 = (SELECT 0 AS Zero FROM t1)
val q2 = (SELECT t1.A < t2.D AS Lt FROM t1, t2)
val q3 = (SELECT t1.A < t2.D AS Lt, t1.A, t2.D, t1.C = t2.A AS Eq FROM t1, t2)
