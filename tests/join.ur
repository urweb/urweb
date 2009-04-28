table t : { A : int }

fun main () =
    r <- oneRow (SELECT * FROM t);
    r <- oneRow (SELECT * FROM t AS T1, t AS T2);
    return <xml/>
