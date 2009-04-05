functor Make(M : sig type t end) : sig
    type topic

    val inj : sql_injectable topic

    val create : transaction topic
    val subscribe : topic -> transaction (channel M.t)
    val send : topic -> M.t -> transaction unit

    val subscribers : topic -> transaction int
end
