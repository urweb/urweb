table t : {A : int, B : int}
  CONSTRAINT UniA UNIQUE A,
  CONSTRAINT UniB UNIQUE B,
  CONSTRAINT UniBoth UNIQUE (A, B),

  CONSTRAINT UniAm UNIQUE {#A},
  CONSTRAINT UniAm2 {unique [#A] [[]] ! !},
  {{one_constraint [#UniAm3] (unique [#A] [[]] ! !)}},

  CONSTRAINT UniBothm UNIQUE ({#A}, {#B}),
  CONSTRAINT UniBothm2 {unique [#A] [[B = _]] ! !},
  {{one_constraint [#UniBothm3] (unique [#A] [[B = _]] ! !)}}

fun main () : transaction page =
    queryI (SELECT * FROM t) (fn _ => return ());
    return <xml/>
