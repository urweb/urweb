fun not b = if b then False else True

con idT (t :: Type) = t
con record (t :: {Type}) = $t
con fstTT (t :: (Type * Type)) = t.1
con sndTT (t :: (Type * Type)) = t.2

con mapTT (f :: Type -> Type) = fold (fn nm t acc [[nm] ~ acc] =>
                                         [nm = f t] ++ acc) []

con mapUT = fn f :: Type => fold (fn nm t acc [[nm] ~ acc] =>
                                     [nm = f] ++ acc) []

con mapT2T (f :: (Type * Type) -> Type) = fold (fn nm t acc [[nm] ~ acc] =>
                                                   [nm = f t] ++ acc) []

con ex = fn tf :: (Type -> Type) =>
            res ::: Type -> (choice :: Type -> tf choice -> res) -> res

fun ex (tf :: (Type -> Type)) (choice :: Type) (body : tf choice) : ex tf =
 fn (res ::: Type) (f : choice :: Type -> tf choice -> res) =>
    f [choice] body

fun compose (t1 ::: Type) (t2 ::: Type) (t3 ::: Type)
            (f1 : t2 -> t3) (f2 : t1 -> t2) (x : t1) = f1 (f2 x)

fun txt (t ::: Type) (ctx ::: {Unit}) (use ::: {Type}) (sh : show t) (v : t) =
    cdata (@show sh v)

fun foldUR (tf :: Type) (tr :: {Unit} -> Type)
           (f : nm :: Name -> rest :: {Unit}
                -> fn [[nm] ~ rest] =>
                      tf -> tr rest -> tr ([nm] ++ rest))
           (i : tr []) =
    fold [fn r :: {Unit} => $(mapUT tf r) -> tr r]
             (fn (nm :: Name) (t :: Unit) (rest :: {Unit}) acc
                              [[nm] ~ rest] r =>
                 f [nm] [rest] r.nm (acc (r -- nm)))
             (fn _ => i)

fun foldUR2 (tf1 :: Type) (tf2 :: Type) (tr :: {Unit} -> Type)
           (f : nm :: Name -> rest :: {Unit}
                -> fn [[nm] ~ rest] =>
                      tf1 -> tf2 -> tr rest -> tr ([nm] ++ rest))
           (i : tr []) =
    fold [fn r :: {Unit} => $(mapUT tf1 r) -> $(mapUT tf2 r) -> tr r]
             (fn (nm :: Name) (t :: Unit) (rest :: {Unit}) acc
                              [[nm] ~ rest] r1 r2 =>
                 f [nm] [rest] r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
             (fn _ _ => i)

fun foldURX2 (tf1 :: Type) (tf2 :: Type) (ctx :: {Unit})
           (f : nm :: Name -> rest :: {Unit}
                -> fn [[nm] ~ rest] =>
                      tf1 -> tf2 -> xml ctx [] []) =
    foldUR2 [tf1] [tf2] [fn _ => xml ctx [] []]
            (fn (nm :: Name) (rest :: {Unit}) [[nm] ~ rest] v1 v2 acc =>
                <xml>{f [nm] [rest] v1 v2}{acc}</xml>)
            <xml/>

fun foldTR (tf :: Type -> Type) (tr :: {Type} -> Type)
           (f : nm :: Name -> t :: Type -> rest :: {Type}
                -> fn [[nm] ~ rest] =>
                      tf t -> tr rest -> tr ([nm = t] ++ rest))
           (i : tr []) =
    fold [fn r :: {Type} => $(mapTT tf r) -> tr r]
             (fn (nm :: Name) (t :: Type) (rest :: {Type}) (acc : _ -> tr rest)
                              [[nm] ~ rest] r =>
                 f [nm] [t] [rest] r.nm (acc (r -- nm)))
             (fn _ => i)

fun foldT2R (tf :: (Type * Type) -> Type) (tr :: {(Type * Type)} -> Type)
            (f : nm :: Name -> t :: (Type * Type) -> rest :: {(Type * Type)}
                 -> fn [[nm] ~ rest] =>
                       tf t -> tr rest -> tr ([nm = t] ++ rest))
            (i : tr []) =
    fold [fn r :: {(Type * Type)} => $(mapT2T tf r) -> tr r]
             (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                              (acc : _ -> tr rest) [[nm] ~ rest] r =>
                 f [nm] [t] [rest] r.nm (acc (r -- nm)))
             (fn _ => i)

fun foldTR2 (tf1 :: Type -> Type) (tf2 :: Type -> Type) (tr :: {Type} -> Type)
            (f : nm :: Name -> t :: Type -> rest :: {Type}
                 -> fn [[nm] ~ rest] =>
                       tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
            (i : tr []) =
    fold [fn r :: {Type} => $(mapTT tf1 r) -> $(mapTT tf2 r) -> tr r]
             (fn (nm :: Name) (t :: Type) (rest :: {Type})
                              (acc : _ -> _ -> tr rest) [[nm] ~ rest] r1 r2 =>
                 f [nm] [t] [rest] r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
             (fn _ _ => i)

fun foldT2R2 (tf1 :: (Type * Type) -> Type) (tf2 :: (Type * Type) -> Type)
             (tr :: {(Type * Type)} -> Type)
             (f : nm :: Name -> t :: (Type * Type) -> rest :: {(Type * Type)}
                  -> fn [[nm] ~ rest] =>
                        tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
             (i : tr []) =
    fold [fn r :: {(Type * Type)} => $(mapT2T tf1 r) -> $(mapT2T tf2 r) -> tr r]
             (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                              (acc : _ -> _ -> tr rest) [[nm] ~ rest] r1 r2 =>
                 f [nm] [t] [rest] r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
             (fn _ _ => i)

fun foldTRX (tf :: Type -> Type) (ctx :: {Unit})
            (f : nm :: Name -> t :: Type -> rest :: {Type}
                 -> fn [[nm] ~ rest] =>
                       tf t -> xml ctx [] []) =
    foldTR [tf] [fn _ => xml ctx [] []]
           (fn (nm :: Name) (t :: Type) (rest :: {Type}) [[nm] ~ rest] r acc =>
               <xml>{f [nm] [t] [rest] r}{acc}</xml>)
           <xml/>

fun foldT2RX (tf :: (Type * Type) -> Type) (ctx :: {Unit})
             (f : nm :: Name -> t :: (Type * Type) -> rest :: {(Type * Type)}
                  -> fn [[nm] ~ rest] =>
                        tf t -> xml ctx [] []) =
    foldT2R [tf] [fn _ => xml ctx [] []]
            (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                             [[nm] ~ rest] r acc =>
                <xml>{f [nm] [t] [rest] r}{acc}</xml>)
            <xml/>

fun foldTRX2 (tf1 :: Type -> Type) (tf2 :: Type -> Type) (ctx :: {Unit})
             (f : nm :: Name -> t :: Type -> rest :: {Type}
                  -> fn [[nm] ~ rest] =>
                        tf1 t -> tf2 t -> xml ctx [] []) =
    foldTR2 [tf1] [tf2] [fn _ => xml ctx [] []]
            (fn (nm :: Name) (t :: Type) (rest :: {Type}) [[nm] ~ rest]
                             r1 r2 acc =>
                <xml>{f [nm] [t] [rest] r1 r2}{acc}</xml>)
            <xml/>

fun foldT2RX2 (tf1 :: (Type * Type) -> Type) (tf2 :: (Type * Type) -> Type)
              (ctx :: {Unit})
              (f : nm :: Name -> t :: (Type * Type) -> rest :: {(Type * Type)}
                   -> fn [[nm] ~ rest] =>
                         tf1 t -> tf2 t -> xml ctx [] []) =
    foldT2R2 [tf1] [tf2] [fn _ => xml ctx [] []]
             (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                              [[nm] ~ rest] r1 r2 acc =>
                 <xml>{f [nm] [t] [rest] r1 r2}{acc}</xml>)
             <xml/>

fun queryX (tables ::: {{Type}}) (exps ::: {Type}) (ctx ::: {Unit})
           (q : sql_query tables exps) [tables ~ exps]
           (f : $(exps ++ fold (fn nm (fields :: {Type}) acc [[nm] ~ acc] =>
                                   [nm = $fields] ++ acc) [] tables)
                -> xml ctx [] []) =
    query q
          (fn fs acc => return <xml>{acc}{f fs}</xml>)
          <xml/>

fun oneOrNoRows (tables ::: {{Type}}) (exps ::: {Type})
                (q : sql_query tables exps) [tables ~ exps] =
    query q
          (fn fs _ => return (Some fs))
          None
