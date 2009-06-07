val mapX : a ::: Type -> b ::: Type -> ctx ::: {Unit}
           -> (a -> b -> xml ctx [] []) -> list a -> list b -> xml ctx [] []

val all : a ::: Type -> b ::: Type -> (a -> b -> bool) -> list a -> list b -> bool
