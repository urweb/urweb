table t : { A : int, B : int }

fun main () =
    x <- queryX (SELECT * FROM t WHERE IF t.A = 6 THEN t.B < 2 ELSE t.B > 5)
                (fn r => <xml><li>{[r.T.A]}, {[r.T.B]}</li></xml>);
    return <xml><body>{x}</body></xml>
