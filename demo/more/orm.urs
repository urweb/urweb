con link :: Type -> Type

con meta = fn col :: Type => {
	      Link : link col,
	      Inj : sql_injectable col
	      }

functor Table(M : sig
		  con cols :: {Type}
		  val cols : $(map meta cols)
		  constraint [Id] ~ cols
		  val folder : folder cols
	      end) : sig
    type id
    val inj : sql_injectable id
    val id : meta id

    type row = $([Id = id] ++ M.cols)

    val create : $M.cols -> transaction row
    val delete : row -> transaction unit
    val save : row -> transaction unit
    val lookup : id -> transaction (option row)
    val list : transaction (list row)

    con col :: Type -> Type
    val idCol : col id
    val cols : $(map col M.cols)

    type filter
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
