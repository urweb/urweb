val rec main = fn () => <html><body>
        <a link={aux ()}>See another page</a>
</body></html>

and aux = fn () => <html><body>
        <a link={main ()}>Back to square one</a>
</body></html>
