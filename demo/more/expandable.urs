con t :: {Unit} -> Type

val create : ctx ::: {Unit} -> xml ctx [] [] -> transaction (t ctx)
val render : ctx ::: {Unit} -> [[Body] ~ ctx] => t ([Body] ++ ctx) -> xml ([Body] ++ ctx) [] []
val expand : ctx ::: {Unit} -> t ctx -> transaction unit
val collapse : ctx ::: {Unit} -> t ctx -> transaction unit
