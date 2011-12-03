fun remote () =
    s <- source <xml/>;
    return (s, <xml><dyn signal={signal s}/></xml>)

fun main () : transaction page =
    x <- source <xml/>;
    return <xml><body>
      <dyn signal={signal x}/>
      <hr/>
      <button onclick={p <- rpc (remote ());
                       set x p.2;
                       set p.1 <xml>Hi!</xml>}/>
    </body></xml>
