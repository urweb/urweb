fun main () : transaction page =
    if naughtyDebug "hello" = 0 then
        return <xml><body></body></xml>
    else
        error <xml>Uhoh!</xml>

(*fun main () : transaction page =
    let
        val a = naughtyDebug ""
    in
        return <xml><body></body></xml>
    end*)
