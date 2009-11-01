con t :: {Unit} -> Type -> Type

val create : ctx ::: {Unit} -> data ::: Type -> list (data * xml ctx [] [] * bool) -> transaction (t ctx data)
val render : ctx ::: {Unit} -> data ::: Type -> [[Body] ~ ctx] => t ([Body] ++ ctx) data -> xml ([Body] ++ ctx) [] []
val selected : ctx ::: {Unit} -> data ::: Type -> t ctx data -> signal (list data)
