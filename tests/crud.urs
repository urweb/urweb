con colMeta = fn cols :: {Type} => $(mapTT (fn t => {Show : t -> xbody}) cols)

functor Make(M : sig
        con cols :: {Type}
        constraint [Id] ~ cols
        val tab : sql_table ([Id = int] ++ cols)

        val title : string

        val cols : colMeta cols
end) : sig
        val main : unit -> transaction page
end
