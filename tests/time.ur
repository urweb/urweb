table t : { Id : int, Time : time }

val now : time = readError "10/30/08 14:35:42"
val later : time = readError "10/30/08 14:37:42"

fun main () =
    dml (INSERT INTO t (Id, Time) VALUES (42, {now}));
    xml <- queryX (SELECT * FROM t)
           (fn r => <xml>{[r.T.Id]}: {[r.T.Time]}<br/></xml>);
    minMax <- oneRow (SELECT CURRENT_TIMESTAMP AS Cur, MIN(t.Time) AS Min, MAX(t.Time) AS Max FROM t);
    return <xml><body>
      {xml}
      {[now]}, {[now = now]}, {[now = later]}, {[later < now]}, {[now < later]}<br/>
      {[minMax.Cur]}, {[minMax.Min]}, {[minMax.Max]}
    </body></xml>
