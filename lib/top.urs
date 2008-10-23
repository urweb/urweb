val not : bool -> bool

con idT = fn t :: Type => t
con record = fn t :: {Type} => $t
con fstTT = fn t :: (Type * Type) => t.1
con sndTT = fn t :: (Type * Type) => t.2

con mapTT = fn f :: Type -> Type => fold (fn nm t acc [[nm] ~ acc] =>
                                             [nm = f t] ++ acc) []

con mapUT = fn f :: Type => fold (fn nm t acc [[nm] ~ acc] =>
                                     [nm = f] ++ acc) []

con mapT2T = fn f :: (Type * Type) -> Type => fold (fn nm t acc [[nm] ~ acc] =>
                                                       [nm = f t] ++ acc) []

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
             -> tr [] -> r :: {Unit} -> $(mapUT tf r) -> tr r

val foldUR2 : tf1 :: Type -> tf2 :: Type -> tr :: ({Unit} -> Type)
             -> (nm :: Name -> rest :: {Unit}
                 -> fn [[nm] ~ rest] =>
                       tf1 -> tf2 -> tr rest -> tr ([nm] ++ rest))
             -> tr [] -> r :: {Unit} -> $(mapUT tf1 r) -> $(mapUT tf2 r) -> tr r

val foldURX2: tf1 :: Type -> tf2 :: Type -> ctx :: {Unit}
              -> (nm :: Name -> rest :: {Unit}
                  -> fn [[nm] ~ rest] =>
                        tf1 -> tf2 -> xml ctx [] [])
              -> r :: {Unit} -> $(mapUT tf1 r) -> $(mapUT tf2 r) -> xml ctx [] []

val foldTR : tf :: (Type -> Type) -> tr :: ({Type} -> Type)
             -> (nm :: Name -> t :: Type -> rest :: {Type}
                 -> fn [[nm] ~ rest] =>
                       tf t -> tr rest -> tr ([nm = t] ++ rest))
             -> tr [] -> r :: {Type} -> $(mapTT tf r) -> tr r

val foldT2R : tf :: ((Type * Type) -> Type) -> tr :: ({(Type * Type)} -> Type)
              -> (nm :: Name -> t :: (Type * Type) -> rest :: {(Type * Type)}
                  -> fn [[nm] ~ rest] =>
                        tf t -> tr rest -> tr ([nm = t] ++ rest))
              -> tr [] -> r :: {(Type * Type)} -> $(mapT2T tf r) -> tr r

val foldTR2 : tf1 :: (Type -> Type) -> tf2 :: (Type -> Type)
              -> tr :: ({Type} -> Type)
              -> (nm :: Name -> t :: Type -> rest :: {Type}
                  -> fn [[nm] ~ rest] =>
                        tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
              -> tr []
              -> r :: {Type} -> $(mapTT tf1 r) -> $(mapTT tf2 r) -> tr r
                                                                    
val foldT2R2 : tf1 :: ((Type * Type) -> Type) -> tf2 :: ((Type * Type) -> Type)
               -> tr :: ({(Type * Type)} -> Type)
               -> (nm :: Name -> t :: (Type * Type) -> rest :: {(Type * Type)}
                   -> fn [[nm] ~ rest] =>
                         tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
               -> tr [] -> r :: {(Type * Type)}
               -> $(mapT2T tf1 r) -> $(mapT2T tf2 r) -> tr r

val foldTRX : tf :: (Type -> Type) -> ctx :: {Unit}
              -> (nm :: Name -> t :: Type -> rest :: {Type}
                  -> fn [[nm] ~ rest] =>
                        tf t -> xml ctx [] [])
              -> r :: {Type} -> $(mapTT tf r) -> xml ctx [] []

val foldT2RX : tf :: ((Type * Type) -> Type) -> ctx :: {Unit}
               -> (nm :: Name -> t :: (Type * Type) -> rest :: {(Type * Type)}
                   -> fn [[nm] ~ rest] =>
                         tf t -> xml ctx [] [])
               -> r :: {(Type * Type)} -> $(mapT2T tf r) -> xml ctx [] []

val foldTRX2 : tf1 :: (Type -> Type) -> tf2 :: (Type -> Type) -> ctx :: {Unit}
               -> (nm :: Name -> t :: Type -> rest :: {Type}
                   -> fn [[nm] ~ rest] =>
                         tf1 t -> tf2 t -> xml ctx [] [])
               -> r :: {Type}
               -> $(mapTT tf1 r) -> $(mapTT tf2 r) -> xml ctx [] []

val foldT2RX2 : tf1 :: ((Type * Type) -> Type) -> tf2 :: ((Type * Type) -> Type)
                -> ctx :: {Unit}
                -> (nm :: Name -> t :: (Type * Type) -> rest :: {(Type * Type)}
                    -> fn [[nm] ~ rest] =>
                          tf1 t -> tf2 t -> xml ctx [] [])
                -> r :: {(Type * Type)}
                -> $(mapT2T tf1 r) -> $(mapT2T tf2 r) -> xml ctx [] []

val queryX : tables ::: {{Type}} -> exps ::: {Type} -> ctx ::: {Unit}
             -> sql_query tables exps
             -> fn [tables ~ exps] =>
                   ($(exps ++ fold (fn nm (fields :: {Type}) acc [[nm] ~ acc] =>
                                       [nm = $fields] ++ acc) [] tables)
                    -> xml ctx [] [])
                   -> transaction (xml ctx [] [])

val oneOrNoRows : tables ::: {{Type}} -> exps ::: {Type}
                  -> sql_query tables exps
                  -> fn [tables ~ exps] =>
                        transaction
                            (option
                                 $(exps
                                       ++ fold (fn nm (fields :: {Type}) acc
                                                      [[nm] ~ acc] =>
                                                   [nm = $fields] ++ acc)
                                                   [] tables))
