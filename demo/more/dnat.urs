type t

val zero : transaction t
val inc : t -> transaction unit
val dec : t -> transaction unit

val render : ctx ::: {Unit} -> inp ::: {Type} -> [[Body] ~ ctx] =>
    xml ([Body] ++ ctx) inp [] -> t -> xml ([Body] ++ ctx) inp []
