table t : {A : int, B : int}
  CONSTRAINT UniA UNIQUE A,
  CONSTRAINT UniB UNIQUE B,
  CONSTRAINT UniBoth UNIQUE (A, B),

  CONSTRAINT UniAm UNIQUE {#A},
  CONSTRAINT UniAm2 UNIQUE {{[A = _]}},
  CONSTRAINT UniAm3 {unique [[A = _]] !},
  {{one_constraint [#UniAm4] (unique [[A = _]] !)}}

fun main () : transaction page =
    queryI (SELECT * FROM t) (fn _ => return ());
    return <xml/>
