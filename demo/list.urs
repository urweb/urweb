datatype list t = Nil | Cons of t * list t

val length : t ::: Type -> list t -> int

val rev : t ::: Type -> list t -> list t
