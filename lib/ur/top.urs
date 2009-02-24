(** Row folding *)

con folder = K ==> fn r :: {K} =>
                      tf :: ({K} -> Type)
                      -> (nm :: Name -> v :: K -> r :: {K} -> tf r
                          -> fn [[nm] ~ r] => tf ([nm = v] ++ r))
                      -> tf [] -> tf r

structure Folder : sig
    val nil : K --> folder (([]) :: {K})
    val cons : K --> r ::: {K} -> nm :: Name -> v :: K
               -> fn [[nm] ~ r] => folder r -> folder ([nm = v] ++ r)
    val concat : K --> r1 ::: {K} -> r2 ::: {K}
                 -> fn [r1 ~ r2] => folder r1 -> folder r2 -> folder (r1 ++ r2)
end


val not : bool -> bool

con idT = fn t :: Type => t
con record = fn t :: {Type} => $t
con fstTT = fn t :: (Type * Type) => t.1
con sndTT = fn t :: (Type * Type) => t.2
con fstTTT = fn t :: (Type * Type * Type) => t.1
con sndTTT = fn t :: (Type * Type * Type) => t.2
con thdTTT = fn t :: (Type * Type * Type) => t.3

con mapUT = fn f :: Type => map (fn _ :: Unit => f)

con ex = fn tf :: (Type -> Type) =>
            res ::: Type -> (choice :: Type -> tf choice -> res) -> res

val ex : tf :: (Type -> Type) -> choice :: Type -> tf choice -> ex tf

val compose : t1 ::: Type -> t2 ::: Type -> t3 ::: Type
              -> (t2 -> t3) -> (t1 -> t2) -> (t1 -> t3)

val txt : t ::: Type -> ctx ::: {Unit} -> use ::: {Type} -> show t -> t
          -> xml ctx use []

val foldUR : tf :: Type -> tr :: ({Unit} -> Type)
             -> (nm :: Name -> rest :: {Unit}
                 -> fn [[nm] ~ rest] =>
                       tf -> tr rest -> tr ([nm] ++ rest))
             -> tr [] -> r ::: {Unit} -> folder r -> $(mapUT tf r) -> tr r

val foldUR2 : tf1 :: Type -> tf2 :: Type -> tr :: ({Unit} -> Type)
             -> (nm :: Name -> rest :: {Unit}
                 -> fn [[nm] ~ rest] =>
                       tf1 -> tf2 -> tr rest -> tr ([nm] ++ rest))
             -> tr [] -> r ::: {Unit} -> folder r -> $(mapUT tf1 r) -> $(mapUT tf2 r) -> tr r

val foldURX2: tf1 :: Type -> tf2 :: Type -> ctx :: {Unit}
              -> (nm :: Name -> rest :: {Unit}
                  -> fn [[nm] ~ rest] =>
                        tf1 -> tf2 -> xml ctx [] [])
              -> r ::: {Unit} -> folder r -> $(mapUT tf1 r) -> $(mapUT tf2 r) -> xml ctx [] []

val foldR : K --> tf :: (K -> Type) -> tr :: ({K} -> Type)
             -> (nm :: Name -> t :: K -> rest :: {K}
                 -> fn [[nm] ~ rest] =>
                       tf t -> tr rest -> tr ([nm = t] ++ rest))
             -> tr [] -> r ::: {K} -> folder r -> $(map tf r) -> tr r

val foldR2 : K --> tf1 :: (K -> Type) -> tf2 :: (K -> Type)
             -> tr :: ({K} -> Type)
             -> (nm :: Name -> t :: K -> rest :: {K}
                 -> fn [[nm] ~ rest] =>
                       tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
             -> tr []
             -> r ::: {K} -> folder r -> $(map tf1 r) -> $(map tf2 r) -> tr r

val foldRX : K --> tf :: (K -> Type) -> ctx :: {Unit}
             -> (nm :: Name -> t :: K -> rest :: {K}
                 -> fn [[nm] ~ rest] =>
                       tf t -> xml ctx [] [])
             -> r ::: {K} -> folder r -> $(map tf r) -> xml ctx [] []

val foldRX2 : K --> tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> ctx :: {Unit}
              -> (nm :: Name -> t :: K -> rest :: {K}
                  -> fn [[nm] ~ rest] =>
                        tf1 t -> tf2 t -> xml ctx [] [])
              -> r ::: {K} -> folder r
              -> $(map tf1 r) -> $(map tf2 r) -> xml ctx [] []

val queryX : tables ::: {{Type}} -> exps ::: {Type} -> ctx ::: {Unit}
             -> sql_query tables exps
             -> fn [tables ~ exps] =>
                   ($(exps ++ map (fn fields :: {Type} => $fields) tables)
                    -> xml ctx [] [])
                   -> transaction (xml ctx [] [])

val queryX' : tables ::: {{Type}} -> exps ::: {Type} -> ctx ::: {Unit}
              -> sql_query tables exps
              -> fn [tables ~ exps] =>
                    ($(exps ++ map (fn fields :: {Type} => $fields) tables)
                     -> transaction (xml ctx [] []))
                    -> transaction (xml ctx [] [])
                       
val oneOrNoRows : tables ::: {{Type}} -> exps ::: {Type}
                  -> sql_query tables exps
                  -> fn [tables ~ exps] =>
                        transaction
                            (option
                                 $(exps
                                       ++ map (fn fields :: {Type} => $fields) tables))

val oneRow : tables ::: {{Type}} -> exps ::: {Type}
             -> sql_query tables exps
             -> fn [tables ~ exps] =>
                   transaction
                       $(exps
                             ++ map (fn fields :: {Type} => $fields) tables)

val eqNullable : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                 -> t ::: Type -> sql_injectable (option t)
                 -> sql_exp tables agg exps (option t)
                 -> sql_exp tables agg exps (option t)
                 -> sql_exp tables agg exps bool

val eqNullable' : tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                  -> t ::: Type -> sql_injectable (option t)
                  -> sql_exp tables agg exps (option t)
                  -> option t
                  -> sql_exp tables agg exps bool
