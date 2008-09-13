con idT = fn t :: Type => t
con record = fn t :: {Type} => $t

con mapTT (f :: Type -> Type) = fold (fn nm t acc => [nm] ~ acc =>
        [nm = f t] ++ acc) []

fun compose (t1 ::: Type) (t2 ::: Type) (t3 ::: Type) (f1 : t2 -> t3) (f2 : t1 -> t2) (x : t1) = f1 (f2 x)

fun txt (t ::: Type) (ctx ::: {Unit}) (use ::: {Type}) (sh : show t) (v : t) = cdata (show sh v)

fun foldTR2 (tf1 :: Type -> Type) (tf2 :: Type -> Type) (tr :: {Type} -> Type)
        (f : nm :: Name -> t :: Type -> rest :: {Type} -> [nm] ~ rest
                -> tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
        (i : tr []) =
        fold [fn r :: {Type} => $(mapTT tf1 r) -> $(mapTT tf2 r) -> tr r]
                (fn (nm :: Name) (t :: Type) (rest :: {Type}) (acc : _ -> _ -> tr rest) =>
                        [[nm] ~ rest] =>
                        fn r1 r2 => f [nm] [t] [rest] r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
                (fn _ _ => i)

fun foldTRX2 (tf1 :: Type -> Type) (tf2 :: Type -> Type) (ctx :: {Unit})
        (f : nm :: Name -> t :: Type -> rest :: {Type} -> [nm] ~ rest
                -> tf1 t -> tf2 t -> xml ctx [] []) =
        foldTR2 [tf1] [tf2] [fn _ => xml ctx [] []]
                (fn (nm :: Name) (t :: Type) (rest :: {Type}) =>
                        [[nm] ~ rest] =>
                        fn r1 r2 acc => <xml>{f [nm] [t] [rest] r1 r2}{acc}</xml>)
                <xml></xml>

fun queryX (tables ::: {{Type}}) (exps ::: {Type}) (ctx ::: {Unit}) (q : sql_query tables exps) =
        [tables ~ exps] =>
        fn (f : $(exps ++ fold (fn nm (fields :: {Type}) acc => [nm] ~ acc => [nm = $fields] ++ acc) [] tables)
                -> xml ctx [] []) =>
        query q
                (fn fs acc => return <xml>{acc}{f fs}</xml>)
                <xml></xml>
