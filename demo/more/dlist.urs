con dlist :: Type -> Type
type position

val create : t ::: Type -> transaction (dlist t)
val clear : t ::: Type -> dlist t -> transaction unit
val append : t ::: Type -> dlist t -> t -> transaction position
val replace : t ::: Type -> dlist t -> list t -> transaction unit

val delete : position -> transaction unit
val elements : t ::: Type -> dlist t -> signal (list t)
val size : t ::: Type -> dlist t -> signal int
val numPassing : t ::: Type -> (t -> signal bool) -> dlist t -> signal int
val foldl : t ::: Type -> acc ::: Type -> (t -> acc -> signal acc) -> acc -> dlist t -> signal acc

val render : ctx ::: {Unit} -> [ctx ~ [Dyn]] => t ::: Type
             -> (t -> position -> xml (ctx ++ [Dyn]) [] [])
             -> {StartPosition : signal (option int),
                 MaxLength : signal (option int),
                 Filter : t -> signal bool,
                 Sort : signal (option (t -> t -> signal bool)) (* <= *)}
             -> dlist t
             -> xml (ctx ++ [Dyn]) [] []
