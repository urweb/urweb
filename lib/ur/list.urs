datatype t = datatype Basis.list

val show : a ::: Type -> show a -> show (list a)

val rev : a ::: Type -> t a -> t a

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b

val mapX : a ::: Type -> ctx ::: {Unit} -> (a -> xml ctx [] []) -> t a -> xml ctx [] []

val mapM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
           -> (a -> m b) -> list a -> m (list b)
