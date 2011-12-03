table t : { A : int, B : string, C : float }
table u : { D : int, E : string, F : float }

val q : transaction (list {T : { A : int, B : string, C : float}, U : { D : int }, X : string }) =
    queryL (SELECT t.*, u.D, 'hi' AS X
            FROM t, u)
