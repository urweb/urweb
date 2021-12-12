val foldlAbort : a ::: Type -> b ::: Type -> c ::: Type
                 -> (a -> b -> c -> option c) -> c -> list a -> list b -> option c

val mapX : a ::: Type -> b ::: Type -> ctx ::: {Unit}
           -> (a -> b -> xml ctx [] []) -> list a -> list b -> xml ctx [] []

val all : a ::: Type -> b ::: Type -> (a -> b -> bool) -> list a -> list b -> bool

val mp : a ::: Type -> b ::: Type -> c ::: Type
         -> (a -> b -> c) -> list a -> list b -> list c

val mapM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type -> c ::: Type
           -> (a -> b -> m c) -> list a -> list b -> m (list c)

val app : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
          -> (a -> b -> m unit) -> list a -> list b -> m unit

val unzip : a ::: Type -> b ::: Type -> list (a * b) -> list a * list b
