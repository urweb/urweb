fun main () : transaction page =
    s <- source [];
    entry <- source "";
    return <xml><body>
      <table>
        <dyn signal={s <- signal s;
                     return (List.mapX (fn s => <xml><tr><td>{[s]}</td></tr></xml>) s)}/>
      </table>

      Add one: <ctextbox source={entry}/> <button onclick={e <- get entry;
                                                           v <- get s;
                                                           set s (e :: v)}/>
    </body></xml>
