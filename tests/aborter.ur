table t : {A : int}

fun main () : transaction page =
    () <- dml (INSERT INTO t (A) VALUES (0));
    return (error <xml>No way, Jose!</xml>)
