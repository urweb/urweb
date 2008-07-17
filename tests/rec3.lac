val rec main = fn () => <html><body>
        <a link={aux ()}>See another page</a>
</body></html>

and aux = fn () => <html><body>
        <h1>The Main Event</h1>

        {auxer ()}
</body></html>

and auxer = fn () => <body>
        <a link={main ()}>Back to square one</a>
</body>
