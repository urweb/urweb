table t : {Id : int, Parent : option int}

fun recurse (root : option int) =
    queryX' (SELECT * FROM t WHERE {eqNullable' (SQL t.Parent) root})
            (fn r =>
                children <- recurse (Some r.T.Id);
                (*s <- source False;*)
                return <xml>
                  <dyn signal={(*v <- signal s;
                               if v then*)
                                   return <xml><b>HI</b>{children}<b>BYE</b></xml>
                               (*else
                                   return <xml/>*)}/>
                </xml>)

fun main () =
    x <- recurse None;
    return <xml><body>{x}</body></xml>
