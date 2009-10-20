con colMeta = fn (db :: Type, state :: Type) =>
                 {Nam : string,
                  Show : db -> xbody,
                  Inject : sql_injectable db,

                  NewState : transaction state,
                  Widget : state -> xbody,
                  ReadState : state -> transaction db}
con colsMeta = fn cols :: {(Type * Type)} => $(map colMeta cols)

val int : string -> colMeta (int, source string)
val float : string -> colMeta (float, source string)
val string : string -> colMeta (string, source string)

functor Make(M : sig
                 con cols :: {(Type * Type)}
                 constraint [Id] ~ cols
                 val fl : folder cols

                 table tab : ([Id = int] ++ map fst cols)

                 val title : string

                 val cols : colsMeta cols
             end) : sig
    val main : unit -> transaction page
end
