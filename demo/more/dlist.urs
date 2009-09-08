con dlist :: Type -> Type
type position

val create : t ::: Type -> transaction (dlist t)
val clear : t ::: Type -> dlist t -> transaction unit
val append : t ::: Type -> dlist t -> t -> transaction position
val delete : position -> transaction unit
val elements : t ::: Type -> dlist t -> signal (list t)

val render : ctx ::: {Unit} -> [ctx ~ body] => t ::: Type
             -> (t -> position -> xml (ctx ++ body) [] [])
             -> dlist t
             -> xml (ctx ++ body) [] []
