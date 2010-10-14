fun ack (m, n) =
    if m = 0 then
        n + 1
    else if n = 0 then
        ack (m - 1, 1)
    else
        ack (m - 1, ack (m, n - 1))

fun main n = return <xml>{[ack (n, 4)]}</xml>
