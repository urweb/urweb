datatype t = datatype Basis.list

val show : a ::: Type -> show a -> show (list a)

val rev : a ::: Type -> t a -> t a

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b

