fun target () = return <xml><body>
  Welcome!
</body></xml>

fun main () = return <xml><body>
  <a link={target ()}>Go there</a>
</body></xml>
