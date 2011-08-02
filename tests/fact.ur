fun fact n = if n <= 1 then 1 else n * fact (n - 1)

fun factTr n acc = if n <= 1 then acc else factTr (n - 1) (n * acc)

fun main () : transaction page = return <xml>{[fact 10]}, {[factTr 10 1]}</xml>
