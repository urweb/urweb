val rec endlessList1 = fn () => <body>
        <li> Buy eggs.</li>
        {endlessList2 ()}
</body>

and endlessList2 = fn () => <body>
        <li> Buy milk.</li>
        {endlessList1 ()}
</body>

val main = fn () => <html><body>
        {endlessList1 ()}
</body></html>
