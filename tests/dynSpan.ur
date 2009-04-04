val x = 1

fun main () : transaction page =
    s <- source "!";
    return <xml><body>
      <dyn signal={x <- signal s; return <xml><span>{[x]}</span></xml>}/>
      <button onclick={x <- get s; set s (x ^ "!")}/>
    </body></xml>
