datatype t = T of variant [A = t]

fun main (x : t) : transaction page = return <xml><body>
  <a link={main x}>Go</a>
</body></xml>
