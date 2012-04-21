table t : {A : int, B : int}
  PRIMARY KEY {{primary_key [#A] [[B = _]]}}

fun main () : transaction page =
    queryI (SELECT * FROM t) (fn _ => return ());
    return <xml/>
