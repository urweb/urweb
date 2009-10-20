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
