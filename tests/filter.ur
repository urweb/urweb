fun filter [fs ::: {Type}] [ks] (t : sql_table fs ks) (p : sql_exp [T = fs] [] [] bool) =
   (SELECT * FROM t WHERE {p})

table t : { A : int, B : float }

task initialize = fn () =>
     dml (INSERT INTO t (A, B) VALUES (1, 2.0));
     dml (INSERT INTO t (A, B) VALUES (2, 1.0));
     dml (INSERT INTO t (A, B) VALUES (3, 3.0));
     dml (INSERT INTO t (A, B) VALUES (4, 4.0));
     dml (INSERT INTO t (A, B) VALUES (44, 4.4))

fun main () : transaction page =
    r <- queryX (filter t (WHERE t.A > 3))
                (fn r => <xml>{[r.T.A]}, {[r.T.B]}; </xml>);
    return <xml><body>{r}</body></xml>
