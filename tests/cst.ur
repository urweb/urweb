table u : {C : int, D : int, E : int}
  PRIMARY KEY C,
  CONSTRAINT U UNIQUE (C, D)

table t : {A : int, B : int}
  PRIMARY KEY B,

  CONSTRAINT UniA UNIQUE A,
  CONSTRAINT UniB UNIQUE B,
  CONSTRAINT UniBoth UNIQUE (A, B),

  CONSTRAINT UniAm UNIQUE {#A},
  CONSTRAINT UniAm2 {unique [#A] [[]] ! !},
  {{one_constraint [#UniAm3] (unique [#A] [[]] ! !)}},

  CONSTRAINT UniBothm UNIQUE ({#A}, {#B}),
  CONSTRAINT UniBothm2 {unique [#A] [[B = _]] ! !},
  {{one_constraint [#UniBothm3] (unique [#A] [[B = _]] ! !)}},

  CONSTRAINT ForA FOREIGN KEY A REFERENCES u (C),
  CONSTRAINT ForAB FOREIGN KEY (A, B) REFERENCES u (D, C) ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT ForBA FOREIGN KEY (A, B) REFERENCES u (C, D) ON UPDATE NO ACTION,

  CONSTRAINT Self FOREIGN KEY B REFERENCES t (B)

table s : {B : option int}
  CONSTRAINT UniB UNIQUE B

table s2 : {B : option int}
  CONSTRAINT ForB FOREIGN KEY B REFERENCES s (B) ON DELETE SET NULL

fun main () : transaction page =
    queryI (SELECT * FROM t) (fn _ => return ());
    return <xml/>
