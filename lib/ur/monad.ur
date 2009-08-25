fun exec [m ::: Type -> Type] (_ : monad m) [ts ::: {Type}] r (fd : folder ts) =
    foldR [m] [fn ts => m $ts]
    (fn [nm :: Name] [v :: Type] [rest :: {Type}] [[nm] ~ rest] action acc =>
        this <- action;
        others <- acc;
        return ({nm = this} ++ others))
    (return {}) [ts] fd r

fun ignore [m ::: Type -> Type] (_ : monad m) [t] (v : m t) = x <- v; return ()

fun foldR [K] [m] (_ : monad m) [tf :: K -> Type] [tr :: {K} -> Type]
          (f : nm :: Name -> t :: K -> rest :: {K}
               -> [[nm] ~ rest] =>
           tf t -> tr rest -> m (tr ([nm = t] ++ rest)))
          (i : tr []) [r :: {K}] (fl : folder r) =
    Top.fold [fn r :: {K} => $(map tf r) -> m (tr r)]
             (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                              (acc : _ -> m (tr rest)) r =>
                 acc' <- acc (r -- nm);
                 f [nm] [t] [rest] ! r.nm acc')
             (fn _ => return i)
             [_] fl

fun foldR2 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tr :: {K} -> Type]
           (f : nm :: Name -> t :: K -> rest :: {K}
                -> [[nm] ~ rest] =>
            tf1 t -> tf2 t -> tr rest -> m (tr ([nm = t] ++ rest)))
           (i : tr []) [r :: {K}] (fl : folder r) =
    Top.fold [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> m (tr r)]
       (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                        (acc : _ -> _ -> m (tr rest)) r1 r2 =>
           acc' <- acc (r1 -- nm) (r2 -- nm);
           f [nm] [t] [rest] ! r1.nm r2.nm acc')
       (fn _ _ => return i)
       [_] fl

fun foldR3 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [tr :: {K} -> Type]
           (f : nm :: Name -> t :: K -> rest :: {K}
                -> [[nm] ~ rest] =>
            tf1 t -> tf2 t -> tf3 t -> tr rest -> m (tr ([nm = t] ++ rest)))
           (i : tr []) [r :: {K}] (fl : folder r) =
    Top.fold [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r) -> m (tr r)]
       (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                        (acc : _ -> _ -> _ -> m (tr rest)) r1 r2 r3 =>
           acc' <- acc (r1 -- nm) (r2 -- nm) (r3 -- nm);
           f [nm] [t] [rest] ! r1.nm r2.nm r3.nm acc')
       (fn _ _ _ => return i)
       [_] fl

fun mapR [K] [m] (_ : monad m) [tf :: K -> Type] [tr :: K -> Type]
         (f : nm :: Name -> t :: K -> tf t -> m (tr t)) =
    @@foldR [m] _ [tf] [fn r => $(map tr r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] (v : tf t)
                     (acc : $(map tr rest)) =>
        v' <- f [nm] [t] v;
        return (acc ++ {nm = v'}))
    {}
