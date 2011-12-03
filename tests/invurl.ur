val r = { F = fn () => return <xml/> }

fun main () : transaction page = return <xml><body>
  <a link={r.F ()}>Go</a>
</body></xml>

fun main' (r' : {F : unit -> transaction page}) : transaction page = return <xml><body>
  <a link={r'.F ()}>Go</a>
</body></xml>
