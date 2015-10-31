val exec : m ::: (Type -> Type) -> monad m -> ts ::: {Type}
           -> $(map m ts) -> folder ts -> m $ts

val ignore : m ::: (Type -> Type) -> monad m -> t ::: Type
             -> m t -> m unit

val mp : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
         -> (a -> b) -> m a -> m b

val liftM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
            -> (a -> b) -> m a -> m b
(* Haskell-style synonym for [mp] *)

val liftM2 : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type -> c ::: Type
             -> (a -> b -> c) -> m a -> m b -> m c

val foldR : K --> m ::: (Type -> Type) -> monad m
            -> tf :: (K -> Type)
            -> tr :: ({K} -> Type)
            -> (nm :: Name -> t :: K -> rest :: {K}
                -> [[nm] ~ rest] =>
                tf t -> tr rest -> m (tr ([nm = t] ++ rest)))
            -> tr []
            -> r ::: {K} -> folder r -> $(map tf r) -> m (tr r)

val foldR2 : K --> m ::: (Type -> Type) -> monad m
             -> tf1 :: (K -> Type) -> tf2 :: (K -> Type)
             -> tr :: ({K} -> Type)
             -> (nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf1 t -> tf2 t -> tr rest -> m (tr ([nm = t] ++ rest)))
             -> tr []
             -> r ::: {K} -> folder r -> $(map tf1 r) -> $(map tf2 r) -> m (tr r)

val foldR3 : K --> m ::: (Type -> Type) -> monad m
             -> tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type)
             -> tr :: ({K} -> Type)
             -> (nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf1 t -> tf2 t -> tf3 t -> tr rest -> m (tr ([nm = t] ++ rest)))
             -> tr []
             -> r ::: {K} -> folder r -> $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r) -> m (tr r)

val mapR0 : K --> m ::: (Type -> Type) -> monad m
           -> tr :: (K -> Type)
           -> (nm :: Name -> t :: K -> m (tr t))
           -> r ::: {K} -> folder r -> m ($(map tr r))

val mapR : K --> m ::: (Type -> Type) -> monad m
           -> tf :: (K -> Type)
           -> tr :: (K -> Type)
           -> (nm :: Name -> t :: K -> tf t -> m (tr t))
           -> r ::: {K} -> folder r -> $(map tf r) -> m ($(map tr r))

val mapR2 : K --> m ::: (Type -> Type) -> monad m
            -> tf1 :: (K -> Type) -> tf2 :: (K -> Type)
            -> tr :: (K -> Type)
            -> (nm :: Name -> t :: K -> tf1 t -> tf2 t -> m (tr t))
            -> r ::: {K} -> folder r -> $(map tf1 r) -> $(map tf2 r) -> m ($(map tr r))

val mapR3 : K --> m ::: (Type -> Type) -> monad m
            -> tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type)
            -> tr :: (K -> Type)
            -> (nm :: Name -> t :: K -> tf1 t -> tf2 t -> tf3 t -> m (tr t))
            -> r ::: {K} -> folder r -> $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r) -> m ($(map tr r))

val foldMapR : K --> m ::: (Type -> Type) -> monad m
               -> tf :: (K -> Type)
               -> tf' :: (K -> Type)
               -> tr :: ({K} -> Type)
               -> (nm :: Name -> t :: K -> rest :: {K}
                   -> [[nm] ~ rest] =>
                   tf t -> tr rest -> m (tf' t * tr ([nm = t] ++ rest)))
               -> tr []
               -> r ::: {K} -> folder r -> $(map tf r) -> m ($(map tf' r) * tr r)

val appR : K --> m ::: (Type -> Type) -> monad m
           -> tf :: (K -> Type)
           -> (nm :: Name -> t :: K -> tf t -> m unit)
           -> r ::: {K} -> folder r -> $(map tf r) -> m unit

val appR2 : K --> m ::: (Type -> Type) -> monad m
             -> tf1 :: (K -> Type) -> tf2 :: (K -> Type)
             -> (nm :: Name -> t :: K -> tf1 t -> tf2 t -> m unit)
             -> r ::: {K} -> folder r -> $(map tf1 r) -> $(map tf2 r) -> m unit

val appR3 : K --> m ::: (Type -> Type) -> monad m
             -> tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type)
             -> (nm :: Name -> t :: K -> tf1 t -> tf2 t -> tf3 t -> m unit)
             -> r ::: {K} -> folder r -> $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r) -> m unit
