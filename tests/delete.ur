table t1 : {A : int, B : string, C : float, D : bool}

fun main () : transaction page =
        () <- dml (delete t1 (WHERE T.A = 5));
        return <html><body>Deleted.</body></html>
