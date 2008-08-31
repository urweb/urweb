val snippet = fn s => <body>
        <h1>{cdata s}</h1>
</body>

val main = fn () => <html><body>
        {snippet "<Hi."}
        {snippet "Bye."}
</body></html>
