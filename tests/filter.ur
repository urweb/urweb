fun filter [fs ::: {Type}] [ks] (t : sql_table fs ks) (p : sql_exp [T = fs] [] [] bool)
   : sql_query [T = fs] [] =
   (SELECT * FROM t WHERE {p})

table t : { A : int, B : float }

fun main () =
    queryX (filter t (WHERE t.A > 3))
           (fn r => <xml>{[r.T.A]}, {[r.T.B]}</xml>)
