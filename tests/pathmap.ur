val x = List.rev (List.Cons (1, List.Cons (0, List.Nil)))
val y = List.mp (plus 2) x

fun main () : transaction page = return <xml><body>
  {[x]}<br/>
  {[y]}<br/>
  {[Aux.hello]}<br/>
  {List.mapX (fn n => <xml>{[n]}!</xml>) x}
</body></xml>
