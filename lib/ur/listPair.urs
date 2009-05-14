val mapX : a ::: Type -> b ::: Type -> ctx ::: {Unit}
           -> (a -> b -> xml ctx [] []) -> list a -> list b -> xml ctx [] []
