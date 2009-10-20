con meta = fn (db :: Type, widget :: Type) =>
                    {Show : db -> xbody,
                     Widget : nm :: Name -> xml form [] [nm = widget],
                     WidgetPopulated : nm :: Name -> db -> xml form [] [nm = widget],
                     Parse : widget -> db,
                     Inject : sql_injectable db}

val int : meta (int, string)
val float : meta (float, string)
val string : meta (string, string)
val bool : meta (bool, bool)

functor Make(M : sig
                 con paper :: {(Type * Type)}
                 constraint [Id, Title] ~ paper
                 val paper : $(map meta paper)

                 con review :: {(Type * Type)}
                 constraint [Paper, User] ~ review
                 val review : $(map meta review)
             end) : sig

    val main : unit -> transaction page

end
