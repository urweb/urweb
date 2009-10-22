con meta = fn (db :: Type, widget :: Type) =>
                    {Nam : string,
                     Show : db -> xbody,
                     Widget : nm :: Name -> xml form [] [nm = widget],
                     WidgetPopulated : nm :: Name -> db -> xml form [] [nm = widget],
                     Parse : widget -> db,
                     Inject : sql_injectable db}

val int : string -> meta (int, string)
val float : string -> meta (float, string)
val string : string -> meta (string, string)
val bool : string -> meta (bool, bool)

val textarea : string -> meta (string, string)

val allContent : ts ::: {(Type * Type)} -> $(map meta ts) -> $(map fst ts) -> folder ts -> xbody

val allWidgets : ts ::: {(Type * Type)} -> $(map meta ts) -> folder ts
                 -> xml form [] (map snd ts)

val allPopulatedTr : ts ::: {(Type * Type)} -> $(map meta ts) -> $(map fst ts) -> folder ts
                     -> xml ([Tr] ++ form) [] (map snd ts)

val ensql : avail ::: {{Type}} -> ts ::: {(Type * Type)} -> $(map meta ts) -> $(map snd ts) -> folder ts
            -> $(map (sql_exp avail [] []) (map fst ts))
