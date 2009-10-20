con reviewMeta = fn (db :: Type, widget :: Type) =>
                    {Show : db -> xbody,
                     Widget : nm :: Name -> xml form [] [nm = widget],
                     WidgetPopulated : nm :: Name -> db -> xml form [] [nm = widget],
                     Parse : widget -> db,
                     Inject : sql_injectable db}

val int : reviewMeta (int, string)
val float : reviewMeta (float, string)
val string : reviewMeta (string, string)
val bool : reviewMeta (bool, bool)

functor Make(M : sig
                 con review :: {(Type * Type)}
                 val review : $(map reviewMeta review)
             end) : sig

    val main : unit -> transaction page

end
