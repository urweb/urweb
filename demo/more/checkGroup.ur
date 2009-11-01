con t ctx data = list (data * xml ctx [] [] * source bool)

fun create [ctx] [data] (items : list (data * xml ctx [] [] * bool)) =
    List.mapM (fn (d, x, b) => s <- source b; return (d, x, s)) items

fun render [ctx] [data] [[Body] ~ ctx] (t : t ([Body] ++ ctx) data) =
    List.mapX (fn (_, x, s) => <xml><ccheckbox source={s}/> {x}<br/></xml>) t

fun selected [ctx] [data] (t : t ctx data) =
    List.foldlM (fn (d, _, s) ls =>
                    s <- signal s;
                    return (if s then
                                d :: ls
                            else
                                ls)) [] t
