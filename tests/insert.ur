table t1 : {A : int, B : string, C : float, D : bool}

fun main () : transaction page =
        () <- dml (insert t1 {A = 5, B = "6", C = 7.0, D = True});
        return <html><body>Inserted.</body></html>
