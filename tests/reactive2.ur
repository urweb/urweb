fun main () : transaction page =
  x <- source <xml>TEST</xml>;
  set x <xml>HI</xml>;
  return <xml><body>
    <dyn signal={signal x}/>
  </body></xml>
