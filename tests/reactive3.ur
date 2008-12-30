fun main () : transaction page =
  x <- source <xml>TEST</xml>;
  return <xml><body>
    <dyn signal={signal x}/>
    <br/>
    <a onclick={alert "Changing...."; set x <xml>CHANGEUP</xml>}>Oh My</a>
  </body></xml>
