table t : { A : int, B : int, C : int }

fun main () =
    v <- queryX1 (SELECT t.A, t.C
                  FROM t
                  WHERE t.B = (SELECT MAX(U.B) AS M
                               FROM t AS U
                               WHERE U.A = t.A))
         (fn r => <xml>{[r.A]},{[r.C]};</xml>);
    return <xml>{v}</xml>
