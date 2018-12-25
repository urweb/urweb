val page = fn b => return <xml><body>
        {cdata (case b of False => "No!" | True => "Yes!")}
</body></xml>

val main : unit -> transaction page = fn () => return <xml><body>
        <li><a link={page True}>True</a></li>
        <li><a link={page False}>False</a></li>
</body></xml>
