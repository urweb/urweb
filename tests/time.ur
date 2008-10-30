val now : time = readError "10/30/08 14:35:42"
val later : time = readError "10/30/08 14:37:42"

fun main () = return <xml>{[now]}, {[now = now]}, {[now = later]}, {[later < now]}, {[now < later]}</xml>
