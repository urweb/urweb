table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int, E : option string}

val q1 = (SELECT COUNT( * ) AS X FROM t1)
val q2 = (SELECT AVG(t1.A) AS X FROM t1)
val q3 = (SELECT SUM(t1.C) AS X FROM t1)
val q4 = (SELECT MIN(t1.B) AS X, MAX(t1.A) AS Y FROM t1)
val q5 = (SELECT SUM(t1.A) AS X FROM t1 GROUP BY t1.B)
val q6 = (SELECT COUNT(t2.E) AS N FROM t2 GROUP BY t2.D)

fun main () : transaction page =
    xml <- queryX q6 (fn r => <xml>{[r.N]};</xml>);
    return <xml><body>{xml}</body></xml>
