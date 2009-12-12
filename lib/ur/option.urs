datatype t = datatype Basis.option

val eq : a ::: Type -> eq a -> eq (t a)
val ord : a ::: Type -> ord a -> ord (t a)

val isNone : a ::: Type -> t a -> bool
val isSome : a ::: Type -> t a -> bool

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
val bind : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> t b

val get : a ::: Type -> a -> option a -> a
