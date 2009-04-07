table t : {A : int, B : int}
  PRIMARY KEY (A, B)

fun main () : transaction page =
    queryI (SELECT * FROM t) (fn _ => return ());
    return <xml/>
