table t : { A : option int }

fun main () : transaction page =
    queryX (SELECT COALESCE(t.A, 13)
            FROM t)
    (fn r => <xml>{[r.1]},</xml>)
