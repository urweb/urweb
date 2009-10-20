con link :: (Type * Type) -> Type
val noParent : t ::: Type -> link (t, unit)

con meta = fn (col :: Type, parent :: Type) => {
	      Link : link (col, parent),
	      Inj : sql_injectable col
	      }

val local : t :: Type -> sql_injectable t -> meta (t, unit)

functor Table(M : sig
		  con cols :: {(Type * Type)}
		  val cols : $(map meta cols)
		  constraint [Id] ~ cols
		  val folder : folder cols
	      end) : sig
    type id
    type row' = $(map fst M.cols)
    type row = $([Id = id] ++ map fst M.cols)

    val inj : sql_injectable id
    val id : meta (id, row)

    val create : row' -> transaction row
    val delete : row -> transaction unit
    val save : row -> transaction unit
    val lookup : id -> transaction (option row)
    val list : transaction (list row)

    con col :: Type -> Type
    val idCol : col id
    val cols : $(map (fn (colm :: Type, parent :: Type) =>
                         {Col : col colm,
                          Parent : row -> transaction (option parent)}) M.cols)

    type filter
    val find : filter -> transaction (option row)
    val search : filter -> transaction (list row)

    val eq : t ::: Type -> col t -> t -> filter
    val ne : t ::: Type -> col t -> t -> filter
    val lt : t ::: Type -> col t -> t -> filter
    val le : t ::: Type -> col t -> t -> filter
    val gt : t ::: Type -> col t -> t -> filter
    val ge : t ::: Type -> col t -> t -> filter

    val _and : filter -> filter -> filter
    val or : filter -> filter -> filter
end
