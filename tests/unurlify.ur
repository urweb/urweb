datatype list t = Nil | Cons of t * list t

fun handler (ls : list bool) = return <xml/>

fun main () : transaction page = return <xml><body>
  <a link={handler Nil}>!</a>
</body></xml>
