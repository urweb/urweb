fun getOpt (t ::: Type) (o : option t) (v : t) : t =
    case o of
        None => v
      | Some x => x

cookie int : int
cookie float : float

fun main () : transaction page =
    n <- getCookie int;
    n <- return (getOpt n 7);
    sn <- source 6;

    f <- getCookie float;
    f <- return (getOpt f 1.23);
    sf <- source 4.56;

    return <xml><body>
      <dyn signal={n <- signal sn; return <xml>{[n]}</xml>}/>
      <a onclick={set sn n}>CHANGE</a><br/>

      <dyn signal={f <- signal sf; return <xml>{[f]}</xml>}/>
      <a onclick={set sf f}>CHANGE</a><br/>
    </body></xml>
