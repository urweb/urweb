datatype t = datatype Basis.option

val eq : a ::: Type -> eq a -> eq (t a)

val isSome : a ::: Type -> t a -> bool

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
val bind : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> t b
