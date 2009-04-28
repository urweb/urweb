table t : { A : int, B : string }

view v = SELECT t.A AS X FROM t

fun main () =
    rows <- queryX (SELECT * FROM v)
            (fn r => <xml><li>{[r.V.X]}</li></xml>);
    return <xml><body>
      {rows}
    </body></xml>
