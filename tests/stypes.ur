fun main () : transaction page =
    sInt <- source 0;
    return <xml><body>
      <dyn signal={n <- signal sInt; return <xml>{[n]}</xml>}/> <a onclick={set sInt 1}>Change</a><br/>
    </body></xml>
