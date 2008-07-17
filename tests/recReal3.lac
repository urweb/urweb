val rec endlessList1 = fn () => <body>
        <li> Buy eggs.</li>
        {endlessList2 ()}
</body>

and endlessList2 = fn () => <body>
        <li> Buy milk.</li>
        {endlessList1 ()}
        {endlessList3 ()}
</body>

and endlessList3 = fn () => <body>
        <li> Buy goat.</li>
</body>

val main = fn () => <html><body>
        {endlessList1 ()}
</body></html>
