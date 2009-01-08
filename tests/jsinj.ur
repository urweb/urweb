fun getOpt (t ::: Type) (o : option t) (v : t) : t =
    case o of
        None => v
      | Some x => x

datatype color = Red | White | Blue

fun colorToString c =
    case c of
        Red => "R"
      | White => "W"
      | Blue => "B"

val show_color = mkShow colorToString

cookie int : int
cookie float : float
cookie string : string
cookie bool : bool
cookie pair : int * float
cookie option : option int
cookie color : color

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

    p <- getCookie pair;
    p <- return (getOpt p (1, 2.3));
    sp <- source (4, 5.6);

    o <- getCookie option;
    o <- return (getOpt o (Some 1));
    so <- source None;

    c <- getCookie color;
    c <- return (getOpt c White);
    sc <- source Blue;

    return <xml><body>
      <dyn signal={n <- signal sn; return <xml>{[n]}</xml>}/>
      <a onclick={set sn n}>CHANGE</a><br/>

      <dyn signal={f <- signal sf; return <xml>{[f]}</xml>}/>
      <a onclick={set sf f}>CHANGE</a><br/>

      <dyn signal={s <- signal ss; return <xml>{[s]}</xml>}/>
      <a onclick={set ss s}>CHANGE</a><br/>

      <dyn signal={b <- signal sb; return <xml>{[b]}</xml>}/>
      <a onclick={set sb b}>CHANGE</a><br/>

      <dyn signal={p <- signal sp; return <xml>{[p.1]}, {[p.2]}</xml>}/>
      <a onclick={set sp p}>CHANGE</a><br/>

      <dyn signal={o <- signal so; case o of
                                       None => return <xml>None</xml>
                                     | Some x => return <xml>{[x]}</xml>}/>
      <a onclick={set so o}>CHANGE</a><br/>

      <dyn signal={c <- signal sc; return <xml>{[c]}</xml>}/>
      <a onclick={set sc c}>CHANGE</a><br/>
    </body></xml>
