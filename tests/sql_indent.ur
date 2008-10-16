table t1 : {A : int, B : string, C : float}

val q1 = (SELECT *
          FROM t1
          WHERE A = 0)
