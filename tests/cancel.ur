type t = {A : int, B : float, C : string}
type u = {A : int, C : string, D : bool}

fun f (x : t) = x
fun g (x : u) = f x

fun h [ts] [ts ~ [A]] (r : $([A = int] ++ ts)) : $([A = int, B = float] ++ ts) = r
