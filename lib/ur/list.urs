datatype t = datatype Basis.list

val show : a ::: Type -> show a -> show (list a)

val foldl : a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b

val rev : a ::: Type -> t a -> t a

val revAppend : a ::: Type -> t a -> t a -> t a

val append : a ::: Type -> t a -> t a -> t a

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b

val mapPartial : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> t b

val mapX : a ::: Type -> ctx ::: {Unit} -> (a -> xml ctx [] []) -> t a -> xml ctx [] []

val mapM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
           -> (a -> m b) -> t a -> m (t b)

val mapXM : m ::: (Type -> Type) -> monad m -> a ::: Type -> ctx ::: {Unit}
            -> (a -> m (xml ctx [] [])) -> t a -> m (xml ctx [] [])

val filter : a ::: Type -> (a -> bool) -> t a -> t a

val exists : a ::: Type -> (a -> bool) -> t a -> bool

val foldlM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
             -> (a -> b -> m b) -> b -> t a -> m b

val foldlMap : a ::: Type -> b ::: Type -> c ::: Type
               -> (a -> b -> c * b) -> b -> t a -> t c * b

val search : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> option b

val all : a ::: Type -> (a -> bool) -> t a -> bool

val app : m ::: (Type -> Type) -> monad m -> a ::: Type
          -> (a -> m unit) -> t a -> m unit


(** Association lists *)

val assoc : a ::: Type -> b ::: Type -> eq a -> a -> t (a * b) -> option b

val assocAdd : a ::: Type -> b ::: Type -> eq a -> a -> b -> t (a * b) -> t (a * b)
