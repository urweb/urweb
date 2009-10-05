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

    val create : $M.cols -> transaction id
end
