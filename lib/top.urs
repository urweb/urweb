con mapTT = fn f :: Type -> Type => fold (fn nm t acc => [nm] ~ acc =>
        [nm = f t] ++ acc) []

val compose : t1 ::: Type -> t2 ::: Type -> t3 ::: Type
        -> (t2 -> t3) -> (t1 -> t2) -> (t1 -> t3)

val txt : t ::: Type -> ctx ::: {Unit} -> use ::: {Type} -> show t -> t
        -> xml ctx use []
