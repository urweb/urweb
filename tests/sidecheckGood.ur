fun main () : transaction page =
    s <- source 0;
    set s 42;
    return <xml><body>
      <dyn signal={n <- signal s; return (txt n)}/>
    </body></xml>
