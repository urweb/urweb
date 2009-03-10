con colMeta = fn t_state :: (Type * Type) =>
                 {Nam : string,
                  Show : t_state.1 -> xbody,
                  Inject : sql_injectable t_state.1,

                  NewState : transaction t_state.2,
                  Widget : t_state.2 -> xbody,
                  ReadState : t_state.2 -> transaction t_state.1}
con colsMeta = fn cols :: {(Type * Type)} => $(map colMeta cols)

val int : string -> colMeta (int, source string)
val float : string -> colMeta (float, source string)
val string : string -> colMeta (string, source string)

functor Make(M : sig
                 con cols :: {(Type * Type)}
                 constraint [Id] ~ cols
                 val fl : folder cols

                 val tab : sql_table ([Id = int] ++ map fst cols)

                 val title : string

                 val cols : colsMeta cols
             end) : sig
    val main : unit -> transaction page
end
