table t : { Id : int, Time : time }

val now : time = readError "10/30/08 14:35:42"
val later : time = readError "10/30/08 14:37:42"

fun main () =
    xml <- queryX (SELECT * FROM t)
           (fn r => <xml>{[r.T.Id]}: {[r.T.Time]}<br/></xml>);
    return <xml><body>
      {xml}
      {[now]}, {[now = now]}, {[now = later]}, {[later < now]}, {[now < later]}
    </body></xml>
