fun main () : transaction page =
  x <- source <xml>A</xml>;
  y <- source <xml>B</xml>;
  return <xml><body>
    <dyn signal={x <- signal x; y <- signal y; return <xml>{x}, {y}</xml>}/>
    <br/>
    <a onclick={set x <xml>C</xml>}>Change x</a><br/>
    <a onclick={set y <xml>D</xml>}>Change y</a><br/>
  </body></xml>
