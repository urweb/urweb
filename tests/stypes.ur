datatype color = Red | White | Blue

fun c2s c =
    case c of
        Red => "Red"
      | White => "White"
      | Blue => "Blue"

val show_color = mkShow c2s

datatype list a = Nil | Cons of a * list a

fun isNil (t ::: Type) (ls : list t) =
    case ls of
        Nil => True
      | _ => False

fun delist (ls : list string) : xml body [] [] =
    case ls of
        Nil => <xml>Nil</xml>
      | Cons (h, t) => <xml>{[h]} :: {delist t}</xml>

fun main () : transaction page =
    sInt <- source 0;
    sFloat <- source 1.23;
    sBoth <- source (7, 42.1);

    sOpt <- source None;
    sBool <- source True;

    sColor <- source White;
    sList <- source Nil;

    return <xml><body>
      <dyn signal={n <- signal sInt; return <xml>{[n + 3]}</xml>}/> <a onclick={set sInt 1}>Change</a><br/>

      <dyn signal={n <- signal sFloat; return <xml>{[n + 1.0]}</xml>}/> <a onclick={set sFloat 4.56}>Change</a><br/>

      <dyn signal={p <- signal sBoth; return <xml>{[p.1]}, {[p.2]}</xml>}/>;
      <dyn signal={p <- signal sBoth; case p of
                                          (7, _) => return <xml>Initial</xml>
                                        | (fst, snd) => return <xml>{[fst]}, {[snd]}</xml>}/>
      <a onclick={set sBoth (8, 100.001)}>Change</a><br/>

      <dyn signal={o <- signal sOpt; case o of
                                         None => return <xml>None</xml>
                                       | Some n => return <xml>{[n]}</xml>}/>
        <a onclick={set sOpt (Some 7)}>Change</a><br/>

      <dyn signal={b <- signal sBool; return <xml>{[b]}</xml>}/>
      <dyn signal={b <- signal sBool; if b then return <xml>Yes</xml> else return <xml>No</xml>}/>
      <a onclick={set sBool False}>Change</a><br/>

      <dyn signal={c <- signal sColor; return <xml>{[c]}</xml>}/>
      <a onclick={set sColor Red}>Red</a>
      <a onclick={set sColor White}>White</a>
      <a onclick={set sColor Blue}>Blue</a><br/>

      <dyn signal={ls <- signal sList; return <xml>{[isNil ls]}</xml>}/>;
      <dyn signal={ls <- signal sList; return <xml>{delist ls}</xml>}/>
      <a onclick={set sList (Cons ("A", Cons ("B", Nil)))}>Change</a><br/>
    </body></xml>
