table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int}

val q1 = (SELECT * FROM t1)

val q2 = (SELECT * FROM t1, t2)

(*val q3 = (SELECT * FROM t1, t1)*)
val q3 = (SELECT * FROM t1, t1 AS T2)

val q4 = (SELECT * FROM {{t1}} AS T, t1 AS T2)

val q5 = (SELECT t1.A FROM t1)
val q6 = (SELECT t1.B, t1.C, t1.A FROM t1)

val q7 = (SELECT t1.A, t2.A FROM t1, t2)
