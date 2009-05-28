datatype t = datatype Basis.list

val show : a ::: Type -> show a -> show (list a)

val rev : a ::: Type -> t a -> t a

val revAppend : a ::: Type -> t a -> t a -> t a

val append : a ::: Type -> t a -> t a -> t a

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b

val mapPartial : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> t b

val mapX : a ::: Type -> ctx ::: {Unit} -> (a -> xml ctx [] []) -> t a -> xml ctx [] []

val mapM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
           -> (a -> m b) -> list a -> m (list b)

val filter : a ::: Type -> (a -> bool) -> t a -> t a

val exists : a ::: Type -> (a -> bool) -> t a -> bool

val foldlMap : a ::: Type -> b ::: Type -> c ::: Type
               -> (a -> b -> c * b) -> b -> t a -> t c * b
