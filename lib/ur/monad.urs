val exec : m ::: (Type -> Type) -> monad m -> ts ::: {Type}
           -> $(map m ts) -> folder ts -> m $ts

val ignore : m ::: (Type -> Type) -> monad m -> t ::: Type
             -> m t -> m unit
