table t : { A : int }

fun main () : transaction page =
    x <- queryX (SELECT *
                 FROM t
                 ORDER BY RANDOM)
         (fn r => <xml>{[r.T.A]}<br/></xml>);
    return <xml><body>{x}</body></xml>
