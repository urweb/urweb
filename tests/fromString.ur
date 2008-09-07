fun i2s s =
        case stringToInt s of
          None => 0
        | Some n => n

fun main () : transaction page = return <html><body>
        Error = {cdata (show _ (i2s "Error"))}<br/>
        3 = {cdata (show _ (i2s "+3"))}<br/>
</body></html>

