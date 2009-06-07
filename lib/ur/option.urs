datatype t = datatype Basis.option

val isSome : a ::: Type -> t a -> bool

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
