val b1 = 1 = 1
val b2 = "Good" = "Bad"

fun eq_pair (t1 :: Type) (t2 :: Type) (eq1 : eq t1) (eq2 : eq t2) (x : t1 * t2) (y : t1 * t2) =
        x.1 = y.1

val b3 = True <> False
