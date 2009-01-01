fun main () : transaction page =
    sInt <- source 0;
    sFloat <- source 1.23;
    sBoth <- source (7, 42.1);

    sOpt <- source None;

    return <xml><body>
      <dyn signal={n <- signal sInt; return <xml>{[n + 3]}</xml>}/> <a onclick={set sInt 1}>Change</a><br/>

      <dyn signal={n <- signal sFloat; return <xml>{[n + 1.0]}</xml>}/> <a onclick={set sFloat 4.56}>Change</a><br/>

      <dyn signal={p <- signal sBoth; return <xml>{[p.1]}, {[p.2]}</xml>}/> <a onclick={set sBoth (8, 100.001)}>Change</a><br/>
    </body></xml>
