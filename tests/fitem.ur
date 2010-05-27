table t : { A : int, B : string }
table u : { A : int, C : float }

val q : sql_query [] [T = [A = int, B = string], U = [C = option float]] [] =
    (SELECT t.A, t.B, u.C
     FROM {{sql_left_join (FROM t) (FROM u) (WHERE TRUE)}})
