table t1 : {A : int, B : string, C : float}

val q1 = (SELECT *
          FROM t1
          WHERE A = 0)

val a1 = (INSERT INTO t1
          VALUES (0, "1", 2.0))

val a2 = (UPDATE t1
          SET A = 3, B = "4", C = 5.0)
         
val a3 = (DELETE FROM t1
          WHERE B <> "good")
