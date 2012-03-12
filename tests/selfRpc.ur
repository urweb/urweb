fun test () =
    k <- source <xml/>;
    return <xml><button onclick={r <- rpc (test ()); set k r}/></xml>

fun main () : transaction page =
    h <- test ();
    return <xml><body>{h}</body></xml>
