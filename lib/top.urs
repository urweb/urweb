con idT = fn t :: Type => t
con record = fn t :: {Type} => $t

con mapTT = fn f :: Type -> Type => fold (fn nm t acc => [nm] ~ acc =>
        [nm = f t] ++ acc) []

val compose : t1 ::: Type -> t2 ::: Type -> t3 ::: Type
        -> (t2 -> t3) -> (t1 -> t2) -> (t1 -> t3)

val txt : t ::: Type -> ctx ::: {Unit} -> use ::: {Type} -> show t -> t
        -> xml ctx use []

val foldTR : tf :: (Type -> Type) -> tr :: ({Type} -> Type)
        -> (nm :: Name -> t :: Type -> rest :: {Type} -> [nm] ~ rest
                -> tf t -> tr rest -> tr ([nm = t] ++ rest))
        -> tr [] -> r :: {Type} -> $(mapTT tf r) -> tr r

val foldTR2 : tf1 :: (Type -> Type) -> tf2 :: (Type -> Type) -> tr :: ({Type} -> Type)
        -> (nm :: Name -> t :: Type -> rest :: {Type} -> [nm] ~ rest
                -> tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
        -> tr [] -> r :: {Type} -> $(mapTT tf1 r) -> $(mapTT tf2 r) -> tr r

val foldTRX : tf :: (Type -> Type) -> ctx :: {Unit}
        -> (nm :: Name -> t :: Type -> rest :: {Type} -> [nm] ~ rest
                -> tf t -> xml ctx [] [])
        -> r :: {Type} -> $(mapTT tf r) -> xml ctx [] []

val foldTRX2 : tf1 :: (Type -> Type) -> tf2 :: (Type -> Type) -> ctx :: {Unit}
        -> (nm :: Name -> t :: Type -> rest :: {Type} -> [nm] ~ rest
                -> tf1 t -> tf2 t -> xml ctx [] [])
        -> r :: {Type} -> $(mapTT tf1 r) -> $(mapTT tf2 r) -> xml ctx [] []

val queryX : tables ::: {{Type}} -> exps ::: {Type} -> ctx ::: {Unit}
        -> sql_query tables exps -> tables ~ exps
        -> ($(exps ++ fold (fn nm (fields :: {Type}) acc => [nm] ~ acc => [nm = $fields] ++ acc) [] tables)
                -> xml ctx [] [])
        -> transaction (xml ctx [] [])
