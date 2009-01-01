fun getOpt (t ::: Type) (o : option t) (v : t) : t =
    case o of
        None => v
      | Some x => x

cookie int : int
cookie float : float
cookie string : string
cookie bool : bool

fun main () : transaction page =
    n <- getCookie int;
    n <- return (getOpt n 7);
    sn <- source 6;

    f <- getCookie float;
    f <- return (getOpt f 1.23);
    sf <- source 4.56;

    s <- getCookie string;
    s <- return (getOpt s "Hi");
    ss <- source "Bye";

    b <- getCookie bool;
    b <- return (getOpt b True);
    sb <- source False;

    return <xml><body>
      <dyn signal={n <- signal sn; return <xml>{[n]}</xml>}/>
      <a onclick={set sn n}>CHANGE</a><br/>

      <dyn signal={f <- signal sf; return <xml>{[f]}</xml>}/>
      <a onclick={set sf f}>CHANGE</a><br/>

      <dyn signal={s <- signal ss; return <xml>{[s]}</xml>}/>
      <a onclick={set ss s}>CHANGE</a><br/>

      <dyn signal={b <- signal sb; return <xml>{[b]}</xml>}/>
      <a onclick={set sb b}>CHANGE</a><br/>
    </body></xml>
