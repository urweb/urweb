con colMeta = fn t_formT :: (Type * Type) => {
        Nam : string,
        Show : t_formT.1 -> xbody,
        Widget : nm :: Name -> xml form [] [nm = t_formT.2],
        WidgetPopulated : nm :: Name -> t_formT.1 -> xml form [] [nm = t_formT.2],
        Parse : t_formT.2 -> t_formT.1,
        Inject : sql_injectable t_formT.1
}
con colsMeta = fn cols :: {(Type * Type)} => $(Top.mapT2T colMeta cols)

functor Make(M : sig
        con cols :: {(Type * Type)}
        constraint [Id] ~ cols
        val tab : sql_table ([Id = int] ++ mapT2T fstTT cols)

        val title : string

        val cols : colsMeta cols
end) : sig
        val main : unit -> transaction page
end
