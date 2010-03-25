table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int, E : option string}

val q1 : sql_query [] _ _ = (SELECT COUNT( * ) FROM t1)
val q2 : sql_query [] _ _ = (SELECT AVG(t1.A) FROM t1)
val q3 : sql_query [] _ _ = (SELECT SUM(t1.C) FROM t1)
val q4 : sql_query [] _ _ = (SELECT MIN(t1.B), MAX(t1.A) FROM t1)
val q5 : sql_query [] _ _ = (SELECT SUM(t1.A) FROM t1 GROUP BY t1.B)
val q6 = (SELECT COUNT(t2.E) FROM t2 GROUP BY t2.D)

fun main () : transaction page =
    xml <- queryX q6 (fn r => <xml>{[r.1]};</xml>);
    xml2 <- queryX q4 (fn r => <xml>{[r.1]}, {[r.2]};</xml>);
    return <xml><body>{xml}<br/>{xml2}</body></xml>
