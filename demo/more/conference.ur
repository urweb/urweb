con reviewMeta = fn (db :: Type, widget :: Type) =>
                    {Show : db -> xbody,
                     Widget : nm :: Name -> xml form [] [nm = widget],
                     WidgetPopulated : nm :: Name -> db -> xml form [] [nm = widget],
                     Parse : widget -> db,
                     Inject : sql_injectable db}

fun default [t] (sh : show t) (rd : read t) (inj : sql_injectable t) : reviewMeta (t, string) =
    {Show = txt,
     Widget = fn [nm :: Name] => <xml><textbox{nm}/></xml>,
     WidgetPopulated = fn [nm :: Name] n =>
                          <xml><textbox{nm} value={show n}/></xml>,
     Parse = readError,
     Inject = _}

val int = default
val float = default
val string = default
val bool = {Show = txt,
            Widget = fn [nm :: Name] => <xml><checkbox{nm}/></xml>,
            WidgetPopulated = fn [nm :: Name] b =>
                                 <xml><checkbox{nm} checked={b}/></xml>,
            Parse = fn x => x,
            Inject = _}

functor Make(M : sig
                 con review :: {(Type * Type)}
                 val review : $(map reviewMeta review)
             end) = struct

    fun main () = return <xml/>

end
