table t : { A : int }

fun sortEm b =
    queryX1 (SELECT * FROM t ORDER BY t.A {if b then sql_asc else sql_desc})
    (fn r => <xml>{[r.A]}; </xml>)

fun main () : transaction page = return <xml><body>
  <a link={sortEm True}>Ascending</a><br/>
  <a link={sortEm False}>Descending</a>
</body></xml>
