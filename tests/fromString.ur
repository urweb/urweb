fun s2i s =
        case read _ s of
          None => 0
        | Some n => n

fun s2f s =
        case read _ s of
          None => 0.0
        | Some n => n

fun s2s s =
        case read _ s of
          None => "Error"
        | Some s => s

fun s2b s =
        case read _ s of
          None => False
        | Some b => b

fun main () : transaction page = return <html><body>
        Error = {cdata (show _ (s2i "Error"))}<br/>
        3 = {cdata (show _ (s2i "+3"))}<br/>
        <br/>
        Error = {cdata (show _ (s2f "Error"))}<br/>
        98.76 = {cdata (show _ (s2f "98.76"))}<br/>
        <br/>
        Error = {cdata (show _ (s2b "Error"))}<br/>
        False = {cdata (show _ (s2b "false"))}<br/>
        True = {cdata (show _ (s2b "trUE"))}<br/>
        <br/>
        Hi = {cdata (s2s "Hi")}<br/>
</body></html>
