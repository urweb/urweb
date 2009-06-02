val exec : m ::: (Type -> Type) -> monad m -> ts ::: {Type}
           -> $(map m ts) -> folder ts -> m $ts
