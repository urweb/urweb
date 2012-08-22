table t : { A : int }

fun main () : transaction page = return <xml><body>
  {[(SELECT t.A FROM t ORDER BY t.A DESC) : sql_query [] [] _ _]}
</body></xml>
