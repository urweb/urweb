table t1 : {A : int, B : string, C : float, D : bool}

fun main () : transaction page =
        () <- dml (update {B = "6", C = 7.0} t1 (WHERE T.A = 5));
        return <html><body>Updated.</body></html>
