fun zero () = return 0

fun main () : transaction page =
    z <- rpc (zero ());
    return <xml>{[z]}</xml>
