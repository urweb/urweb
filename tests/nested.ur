table t : {A : int, B : int}

fun init () =
    dml (DELETE FROM t WHERE TRUE);
    dml (INSERT INTO t (A, B) VALUES (1, 2));
    dml (INSERT INTO t (A, B) VALUES (2, 3))

fun easy () =
    queryX' (SELECT MAX(t.A) AS M FROM t)
    (fn r =>
        queryX (SELECT * FROM t WHERE t.A = {[r.M]})
        (fn r => <xml>({[r.T.A]}, {[r.T.B]})</xml>))

fun hard id =
    queryX' (SELECT t.B AS N FROM t WHERE t.A = {[id]})
    (fn r =>
        b <- hard r.N;
        return <xml>({[id]}, {[r.N]}); {b}</xml>)

fun doit () =
    init ();
    b1 <- easy ();
    b2 <- hard 1;
    return <xml><body>
      {b1}<br/>
      {b2}
    </body></xml>

fun main () = return <xml><body><form><submit action={doit}/></form></body></xml>
