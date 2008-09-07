table t1 : {A : int, B : string, C : float, D : bool}

fun main () : transaction page =
        () <- dml (DELETE FROM t1 WHERE A = 5);
        return <html><body>Deleted.</body></html>
