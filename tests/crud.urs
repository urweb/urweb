functor Make(M : sig
        con cols :: {Type}
        constraint [Id] ~ cols
        val tab : sql_table ([Id = int] ++ cols)

        val title : string

        val cols : $(mapTT (fn t => {Show : t -> xbody}) cols)
end) : sig
        val main : unit -> transaction page
end
