fun main () : transaction page =
    sInt <- source 0;
    sFloat <- source 1.23;
    sBoth <- source (7, 42.1);

    sOpt <- source None;
    sBool <- source True;

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

      <dyn signal={b <- signal sBool; return <xml>{[b]}</xml>}/> <a onclick={set sBool False}>Change</a><br/>
    </body></xml>
