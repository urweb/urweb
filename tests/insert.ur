table t1 : {A : int, B : string, C : float, D : bool}

fun main () : transaction page =
        () <- dml (INSERT INTO t1 (A, B, C, D) VALUES (5, "6", 7.0, TRUE));
        return <html><body>Inserted.</body></html>
