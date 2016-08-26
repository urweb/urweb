open Json

fun main () : transaction page = return <xml><body>
  {[fromJson "[1, 2, 3]" : list int]}<br/>
  {[toJson ("hi" :: "bye\"" :: "hehe" :: [])]}
</body></xml>
