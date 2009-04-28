table t1 : {A : int, B : string, C : float, D : bool}
table t2 : {A : float, D : int}

datatype list a = Nil | Cons of a * list a

val q1 = (SELECT * FROM t1)

val r1 : transaction (list {A : int, B : string, C : float, D : bool}) =
    query q1
          (fn fs acc => return (Cons (fs.T1, acc)))
          Nil

val r2 : transaction string =
    ls <- r1;
    return (case ls of
                Nil => "Problem"
              | Cons ({B = b, ...}, _) => b)

fun main () : transaction page =
    s <- r2;
    return <xml><body>
      {cdata s}
    </body></xml>
