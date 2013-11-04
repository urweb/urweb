val x = 1 :: 2 :: []
val y = 3 :: 4 :: []

fun main () : transaction page = return <xml>
  {[ListPair.mp plus x y]}
</xml>
