table t : { A : int, B : int, C : int }

fun main () =
    v <- queryX1 (SELECT t.A, t.C
                  FROM t
                  WHERE t.B = (SELECT MAX(U.B) AS M
                               FROM t AS U
                               WHERE U.A = t.A))
                 (fn r => <xml>{[r.A]},{[r.C]};</xml>);
    v' <- queryX1 (SELECT t.A, t.C
                   FROM (SELECT t.A AS A, MAX(t.B) AS B
                         FROM t
                         GROUP BY t.A) AS Maxes
                   JOIN t ON t.A = Maxes.A AND t.B = Maxes.B)
                  (fn r => <xml>{[r.A]},{[r.C]};</xml>);
    return <xml><body>
      {v}<br/>
      {v'}
    </body></xml>
