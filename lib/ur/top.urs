(** Row folding *)

con folder :: K --> {K} -> Type

val fold : K --> tf :: ({K} -> Type)
           -> (nm :: Name -> v :: K -> r :: {K} -> [[nm] ~ r] =>
               tf r -> tf ([nm = v] ++ r))
           -> tf []
           -> r :: {K} -> folder r -> tf r

structure Folder : sig
    val nil : K --> folder (([]) :: {K})
    val cons : K --> r ::: {K} -> nm :: Name -> v :: K
               -> [[nm] ~ r] => folder r -> folder ([nm = v] ++ r)
    val concat : K --> r1 ::: {K} -> r2 ::: {K}
                 -> [r1 ~ r2] => folder r1 -> folder r2 -> folder (r1 ++ r2)
    val mp : K1 --> K2 --> f ::: (K1 -> K2) -> r ::: {K1}
             -> folder r -> folder (map f r)
end


val not : bool -> bool

con idT = fn t :: Type => t
con record = fn t :: {Type} => $t
con fst = K1 ==> K2 ==> fn t :: (K1 * K2) => t.1
con snd = K1 ==> K2 ==> fn t :: (K1 * K2) => t.2
con fst3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.1
con snd3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.2
con thd3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.3

con mapU = K ==> fn f :: K => map (fn _ :: Unit => f)

con ex = fn tf :: (Type -> Type) =>
            res ::: Type -> (choice :: Type -> tf choice -> res) -> res

val ex : tf :: (Type -> Type) -> choice :: Type -> tf choice -> ex tf

val compose : t1 ::: Type -> t2 ::: Type -> t3 ::: Type
              -> (t2 -> t3) -> (t1 -> t2) -> (t1 -> t3)

val txt : t ::: Type -> ctx ::: {Unit} -> use ::: {Type} -> css ::: {Unit} -> show t -> t
          -> xml ctx use [] css

val foldUR : tf :: Type -> tr :: ({Unit} -> Type)
             -> (nm :: Name -> rest :: {Unit}
                 -> [[nm] ~ rest] =>
                       tf -> tr rest -> tr ([nm] ++ rest))
             -> tr [] -> r :: {Unit} -> folder r -> $(mapU tf r) -> tr r

val foldUR2 : tf1 :: Type -> tf2 :: Type -> tr :: ({Unit} -> Type)
             -> (nm :: Name -> rest :: {Unit}
                 -> [[nm] ~ rest] =>
                       tf1 -> tf2 -> tr rest -> tr ([nm] ++ rest))
             -> tr [] -> r :: {Unit} -> folder r -> $(mapU tf1 r) -> $(mapU tf2 r) -> tr r

val foldURX2: css ::: {Unit} -> tf1 :: Type -> tf2 :: Type -> ctx :: {Unit}
              -> (nm :: Name -> rest :: {Unit}
                  -> [[nm] ~ rest] =>
                        tf1 -> tf2 -> xml ctx [] [] css)
              -> r :: {Unit} -> folder r -> $(mapU tf1 r) -> $(mapU tf2 r) -> xml ctx [] [] css

val foldR : K --> tf :: (K -> Type) -> tr :: ({K} -> Type)
             -> (nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf t -> tr rest -> tr ([nm = t] ++ rest))
             -> tr [] -> r :: {K} -> folder r -> $(map tf r) -> tr r

val foldR2 : K --> tf1 :: (K -> Type) -> tf2 :: (K -> Type)
             -> tr :: ({K} -> Type)
             -> (nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
             -> tr []
             -> r :: {K} -> folder r -> $(map tf1 r) -> $(map tf2 r) -> tr r

val foldRX : K --> css ::: {Unit} -> tf :: (K -> Type) -> ctx :: {Unit}
             -> (nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf t -> xml ctx [] [] css)
             -> r :: {K} -> folder r -> $(map tf r) -> xml ctx [] [] css

val foldRX2 : K --> css ::: {Unit} -> tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> ctx :: {Unit}
              -> (nm :: Name -> t :: K -> rest :: {K}
                  -> [[nm] ~ rest] =>
                        tf1 t -> tf2 t -> xml ctx [] [] css)
              -> r :: {K} -> folder r
              -> $(map tf1 r) -> $(map tf2 r) -> xml ctx [] [] css

val queryI : tables ::: {{Type}} -> exps ::: {Type}
             -> [tables ~ exps] =>
             sql_query tables exps
             -> ($(exps ++ map (fn fields :: {Type} => $fields) tables)
                 -> transaction unit)
             -> transaction unit

val queryX : tables ::: {{Type}} -> exps ::: {Type} -> ctx ::: {Unit} -> css ::: {Unit}
             -> [tables ~ exps] =>
             sql_query tables exps
             -> ($(exps ++ map (fn fields :: {Type} => $fields) tables)
                 -> xml ctx [] [] css)
             -> transaction (xml ctx [] [] css)

val queryX' : tables ::: {{Type}} -> exps ::: {Type} -> ctx ::: {Unit} -> css ::: {Unit}
              -> [tables ~ exps] =>
              sql_query tables exps
              -> ($(exps ++ map (fn fields :: {Type} => $fields) tables)
                  -> transaction (xml ctx [] [] css))
              -> transaction (xml ctx [] [] css)
                       
val oneOrNoRows : tables ::: {{Type}} -> exps ::: {Type}
                  -> [tables ~ exps] =>
                  sql_query tables exps
                  -> transaction
                         (option
                              $(exps
                                    ++ map (fn fields :: {Type} => $fields) tables))

val oneRow : tables ::: {{Type}} -> exps ::: {Type}
             -> [tables ~ exps] =>
             sql_query tables exps
             -> transaction
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
