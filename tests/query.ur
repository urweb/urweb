table t1 : {A : int, B : string, C : float}
table t2 : {A : float, D : int}

datatype list a = Nil | Cons of a * list a

val q1 = (SELECT * FROM t1)
val r1 : transaction (list {A : int, B : string, C : float}) =
        query q1
        (fn fs _ acc => return (Cons (fs.T1, acc)))
        Nil

val r2 : transaction int =
        ls <- r1;
        return (case ls of
                    Nil => 0
                  | Cons ({A = a, ...}, _) => a)
