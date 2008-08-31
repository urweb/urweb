table t : {A : int, B : string, C : float}

val q1 = (SELECT * FROM t LIMIT 42)
val q2 = fn n => (SELECT * FROM t LIMIT {n})

val q3 = (SELECT * FROM t OFFSET 3)
val q4 = fn n => fn m => (SELECT * FROM t LIMIT {n} OFFSET {m})
