table t : { A : int }

fun other () = return <xml><body>
  Other
</body></xml>

fun main () = return <xml><body>
  <a link={other ()}>Hi!</a>
</body></xml>
