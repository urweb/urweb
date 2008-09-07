table t1 : {A : int, B : string, C : float, D : bool}

fun main () : transaction page =
        () <- dml (UPDATE t1 SET B = '6', C = 7.0 WHERE A = 5);
        return <html><body>Updated.</body></html>
