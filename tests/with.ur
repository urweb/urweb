val r = ({A = 1, B = 2} with #C = "Hi") with #D = "Bye"

fun main () : transaction page = return <html><body>
        {cdata r.C}, {cdata r.D}
</body></html>
