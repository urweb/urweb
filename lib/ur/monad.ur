fun exec [m ::: Type -> Type] (_ : monad m) [ts ::: {Type}] r (fd : folder ts) =
    foldR [m] [fn ts => m $ts]
    (fn [nm :: Name] [v :: Type] [rest :: {Type}] [[nm] ~ rest] action acc =>
        this <- action;
        others <- acc;
        return ({nm = this} ++ others))
    (return {}) [ts] fd r

fun ignore [m ::: Type -> Type] (_ : monad m) [t] (v : m t) = x <- v; return ()
