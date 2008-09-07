table t1 : {A : int, B : string, C : float, D : bool}

fun main () : transaction page =
        () <- dml (UPDATE t1 SET B = 'Hi', C = 12.34 WHERE A = 5);
        return <html><body>Updated.</body></html>
