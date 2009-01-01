cookie int : int

fun getOpt (t ::: Type) (o : option t) (v : t) : t =
    case o of
        None => v
      | Some x => x

fun main () : transaction page =
    n <- getCookie int;
    sn <- source (getOpt n 7);
    return <xml><body>
      <dyn signal={n <- signal sn; return <xml>{[n]}</xml>}/>
      <a onclick={set sn 6}>CHANGE</a>
    </body></xml>
