val ls = 1 :: 2 :: 3 :: 4 :: 5 :: 6
           :: 1 :: 2 :: 3 :: 4 :: 5 :: 6
           :: 1 :: 2 :: 3 :: 4 :: 5 :: 6
           :: 1 :: 2 :: 3 :: 4 :: 5 :: 6
           :: 1 :: 2 :: 3 :: 4 :: 5 :: 6
           :: 1 :: 2 :: 3 :: 4 :: 5 :: 6
           :: []

fun main () : transaction page = return <xml><body>
  {List.mapX txt ls}<br/>
  {List.mapX txt ls}
</body></xml>
