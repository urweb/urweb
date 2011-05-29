fun action () =
  setHeader (blessResponseHeader "Location") "http://www.google.com/";
  return <xml/>

fun main () =
  ag <- getHeader (blessRequestHeader "User-Agent");
  return <xml><body>
    User agent: {[ag]}

    <form> <submit action={action}/> </form>
  </body></xml>
