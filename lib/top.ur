con mapTT (f :: Type -> Type) = fold (fn nm t acc => [nm] ~ acc =>
        [nm = f t] ++ acc) []

fun compose (t1 ::: Type) (t2 ::: Type) (t3 ::: Type) (f1 : t2 -> t3) (f2 : t1 -> t2) (x : t1) = f1 (f2 x)

fun txt (t ::: Type) (ctx ::: {Unit}) (use ::: {Type}) (sh : show t) (v : t) = cdata (show sh v)
