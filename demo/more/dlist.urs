con dlist :: Type -> Type
type position

val create : t ::: Type -> transaction (dlist t)
val clear : t ::: Type -> dlist t -> transaction unit
val append : t ::: Type -> dlist t -> t -> transaction position
val delete : position -> transaction unit
val elements : t ::: Type -> dlist t -> signal (list t)
val foldl : t ::: Type -> acc ::: Type -> (t -> acc -> signal acc) -> acc -> dlist t -> signal acc

val render : ctx ::: {Unit} -> [ctx ~ body] => t ::: Type
             -> (t -> position -> xml (ctx ++ body) [] [])
             -> {Filter : t -> signal bool,
                 Sort : signal (option (t -> t -> signal bool)) (* <= *)}
             -> dlist t
             -> xml (ctx ++ body) [] []
