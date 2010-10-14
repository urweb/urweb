fun more n =
    if n <= 0 then
        "!"
    else
        more (n-1) ^ more (n-1)

fun main n = return <xml>{[more n]}</xml>
