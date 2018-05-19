table t : { A : int }

fun sortEm b =
    queryX1 (SELECT * FROM t ORDER BY t.A {if b then sql_asc else sql_desc})
    (fn r => <xml>{[r.A]}; </xml>)

task initialize = fn () =>
     dml (INSERT INTO t (A) VALUES (1));
     dml (INSERT INTO t (A) VALUES (2));
     dml (INSERT INTO t (A) VALUES (3))

fun main () : transaction page =
    p1 <- sortEm True;
    p2 <- sortEm False;
    return <xml><body>
  <p>{p1}</p>
  <p>{p2}</p>
</body></xml>
