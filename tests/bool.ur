val page = fn b => <html><body>
        {cdata (case b of False => "No!" | True => "Yes!")}
</body></html>

val main : unit -> page = fn () => <html><body>
        <li><a link={page True}>True</a></li>
        <li><a link={page False}>False</a></li>
</body></html>
