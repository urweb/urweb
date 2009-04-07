con colMeta = fn t_formT :: (Type * Type) =>
                 {Nam : string,
                  Show : t_formT.1 -> xbody,
                  Widget : nm :: Name -> xml form [] [nm = t_formT.2],
                  WidgetPopulated : nm :: Name -> t_formT.1
                                    -> xml form [] [nm = t_formT.2],
                  Parse : t_formT.2 -> t_formT.1,
                  Inject : sql_injectable t_formT.1}
con colsMeta = fn cols :: {(Type * Type)} => $(mapT2T colMeta cols)

val int : string -> colMeta (int, string)
val float : string -> colMeta (float, string)
val string : string -> colMeta (string, string)
val bool : string -> colMeta (bool, bool)

functor Make(M : sig
                 con cols :: {(Type * Type)}
                 constraint [Id] ~ cols
                 table tab : ([Id = int] ++ mapT2T fstTT cols)

                 val title : string

                 val cols : colsMeta cols
             end) : sig
    val main : unit -> transaction page
end
