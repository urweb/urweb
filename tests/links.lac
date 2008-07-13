val pC : xhtml = <html><body>
        <h1>Page C</h1>
</body></html>

val pB : xhtml = <html><body>
        <h1>Page B</h1>

        <li> <a link={pC}>C</a></li>
</body></html>

val pA : xhtml = <html><body>
        <h1>Page A</h1>

        <li> <a link={pB}>B</a></li>
        <li> <a link={pC}>C</a></li>
</body></html>

val main : unit -> xhtml = fn () => <html><body>
        <h1>Main</h1>

        <li> <a link={pA}>A</a></li>
        <li> <a link={pB}>B</a></li>
        <li> <a link={pC}>C</a></li>
</body></html>
