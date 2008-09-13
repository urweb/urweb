con colMeta' = fn t :: Type => {Show : t -> xbody}
con colMeta = fn cols :: {Type} => $(Top.mapTT colMeta' cols)

functor Make(M : sig
        con cols :: {Type}
        constraint [Id] ~ cols
        val tab : sql_table ([Id = int] ++ cols)

        val title : string

        val cols : colMeta cols
end) : sig
        val main : unit -> transaction page
end
