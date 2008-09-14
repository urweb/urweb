sequence seq

fun main () : transaction page =
        n <- nextval seq;
        return <html><body>
                {txt _ n}
        </body></html>
