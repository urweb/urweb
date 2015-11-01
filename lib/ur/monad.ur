fun exec [m ::: Type -> Type] (_ : monad m) [ts ::: {Type}] r (fd : folder ts) =
    @foldR [m] [fn ts => m $ts]
     (fn [nm :: Name] [v :: Type] [rest :: {Type}] [[nm] ~ rest] action acc =>
         this <- action;
         others <- acc;
         return ({nm = this} ++ others))
     (return {}) fd r

fun ignore [m ::: Type -> Type] (_ : monad m) [t] (v : m t) = x <- v; return ()

fun mp [m] (_ : monad m) [a] [b] f m =
    v <- m;
    return (f v)

val liftM = @@mp

fun foldR [K] [m] (_ : monad m) [tf :: K -> Type] [tr :: {K} -> Type]
          (f : nm :: Name -> t :: K -> rest :: {K}
               -> [[nm] ~ rest] =>
           tf t -> tr rest -> m (tr ([nm = t] ++ rest)))
          (i : tr []) [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r :: {K} => $(map tf r) -> m (tr r)]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                      (acc : _ -> m (tr rest)) r =>
         acc' <- acc (r -- nm);
         f [nm] [t] [rest] r.nm acc')
     (fn _ => return i)
     fl

fun foldR2 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tr :: {K} -> Type]
           (f : nm :: Name -> t :: K -> rest :: {K}
                -> [[nm] ~ rest] =>
            tf1 t -> tf2 t -> tr rest -> m (tr ([nm = t] ++ rest)))
           (i : tr []) [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> m (tr r)]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                      (acc : _ -> _ -> m (tr rest)) r1 r2 =>
         acc' <- acc (r1 -- nm) (r2 -- nm);
         f [nm] [t] [rest] r1.nm r2.nm acc')
     (fn _ _ => return i)
     fl

fun foldR3 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [tr :: {K} -> Type]
           (f : nm :: Name -> t :: K -> rest :: {K}
                -> [[nm] ~ rest] =>
            tf1 t -> tf2 t -> tf3 t -> tr rest -> m (tr ([nm = t] ++ rest)))
           (i : tr []) [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r) -> m (tr r)]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                      (acc : _ -> _ -> _ -> m (tr rest)) r1 r2 r3 =>
         acc' <- acc (r1 -- nm) (r2 -- nm) (r3 -- nm);
         f [nm] [t] [rest] r1.nm r2.nm r3.nm acc')
     (fn _ _ _ => return i)
     fl

fun mapR0 [K] [m] (_ : monad m) [tr :: K -> Type]
         (f : nm :: Name -> t :: K -> m (tr t)) [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r => m ($(map tr r))]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] (acc : m ($(map tr rest))) =>
        v <- f [nm] [t];
        vs <- acc;
        return (vs ++ {nm = v}))
    (return {})
    fl

fun mapR [K] [m] (_ : monad m) [tf :: K -> Type] [tr :: K -> Type]
         (f : nm :: Name -> t :: K -> tf t -> m (tr t)) =
    @@foldR [m] _ [tf] [fn r => $(map tr r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] (v : tf t)
                     (acc : $(map tr rest)) =>
        v' <- f [nm] [t] v;
        return (acc ++ {nm = v'}))
    {}

fun mapR2 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tr :: K -> Type]
         (f : nm :: Name -> t :: K -> tf1 t -> tf2 t -> m (tr t)) =
    @@foldR2 [m] _ [tf1] [tf2] [fn r => $(map tr r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] (v1 : tf1 t) (v2 : tf2 t)
                     (acc : $(map tr rest)) =>
        v' <- f [nm] [t] v1 v2;
        return (acc ++ {nm = v'}))
    {}

fun mapR3 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [tr :: K -> Type]
         (f : nm :: Name -> t :: K -> tf1 t -> tf2 t -> tf3 t -> m (tr t)) =
    @@foldR3 [m] _ [tf1] [tf2] [tf3] [fn r => $(map tr r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] (v1 : tf1 t) (v2 : tf2 t) (v3 : tf3 t)
                     (acc : $(map tr rest)) =>
        v' <- f [nm] [t] v1 v2 v3;
        return (acc ++ {nm = v'}))
    {}

fun foldMapR [K] [m] (_ : monad m) [tf :: K -> Type] [tf' :: K -> Type] [tr :: {K} -> Type]
             (f : nm :: Name -> t :: K -> rest :: {K}
                  -> [[nm] ~ rest] =>
              tf t -> tr rest -> m (tf' t * tr ([nm = t] ++ rest)))
             (i : tr []) [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r :: {K} => $(map tf r) -> m ($(map tf' r) * tr r)]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                      (acc : _ -> m ($(map tf' rest) * tr rest)) r =>
         p <- acc (r -- nm);
         p' <- f [nm] [t] [rest] r.nm p.2;
         return ({nm = p'.1} ++ p.1, p'.2))
     (fn _ => return ({}, i))
     fl

fun appR [K] [m] (_ : monad m) [tf :: K -> Type]
           (f : nm :: Name -> t :: K -> tf t -> m unit)
           [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r :: {K} => $(map tf r) -> m unit]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] acc r =>
         acc (r -- nm);
         f [nm] [t] r.nm)
     (fn _ => return ())
     fl

fun appR2 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type]
           (f : nm :: Name -> t :: K -> tf1 t -> tf2 t -> m unit)
           [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> m unit]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] acc r1 r2 =>
         acc (r1 -- nm) (r2 -- nm);
         f [nm] [t] r1.nm r2.nm)
     (fn _ _ => return ())
     fl

fun appR3 [K] [m] (_ : monad m) [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
           (f : nm :: Name -> t :: K -> tf1 t -> tf2 t -> tf3 t -> m unit)
           [r ::: {K}] (fl : folder r) =
    @Top.fold [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r) -> m unit]
     (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] acc r1 r2 r3 =>
         acc (r1 -- nm) (r2 -- nm) (r3 -- nm);
         f [nm] [t] r1.nm r2.nm r3.nm)
     (fn _ _ _ => return ())
     fl

fun liftM2 [m ::: Type -> Type] (_ : monad m) [a] [b] [c] (f : a -> b -> c) (mx : m a) (my : m b) : m c =
    x <- mx;
    y <- my;
    return (f x y)
