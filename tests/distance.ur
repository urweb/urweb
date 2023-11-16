table t : { A : option string, B : option string}

fun main () : transaction page =
    queryX (SELECT t.A <-> t.B AS X
            FROM t)
           (fn r => <xml>{[r.X]}</xml>)
