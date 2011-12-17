table lower : { A : string }

fun main () : transaction page =
    all <- queryX1 (SELECT *
                    FROM lower
                    WHERE lower(lower.A) LIKE '%foo')
                   (fn r => <xml>{[r.A]}<br/></xml>);
    return <xml><body>
      {all}
    </body></xml>
