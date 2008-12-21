fun main () : transaction page =
  x <- source <xml>TEST</xml>;
  return <xml><body>
    <dyn signal={signal x}/>
  </body></xml>
