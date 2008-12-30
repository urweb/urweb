fun main () : transaction page =
  x <- source <xml>TEST</xml>;
  return <xml><body>
    <dyn signal={y <- signal x; return <xml>!{y}?</xml>}/>
    <br/>
    <a onclick={set x <xml>CHANGEUP</xml>}>Oh My</a>
  </body></xml>
