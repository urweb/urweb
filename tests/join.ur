table t : { A : int, B : string, C : option string }

fun main () =
    r <- oneRow (SELECT * FROM t);
    r <- oneRow (SELECT * FROM t AS T1, t AS T2);
    r <- oneRow (SELECT * FROM t AS T1 CROSS JOIN t AS T2);
    r <- oneRow (SELECT * FROM t AS T1 JOIN t AS T2 ON T1.A = T2.A);
    r <- oneRow (SELECT * FROM t AS T1 LEFT JOIN t AS T2 ON T1.A = T2.A);
    r <- oneRow (SELECT * FROM t AS T1 RIGHT OUTER JOIN t AS T2 ON T1.A = T2.A);
    r <- oneRow (SELECT * FROM t AS T1 FULL JOIN t AS T2 ON T1.A = T2.A);
    return <xml/>
