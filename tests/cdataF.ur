val snippet = fn s => <xml>
        <h1>{cdata s}</h1>
</xml>

val main : unit -> transaction page = fn () => return <xml><body>
        {snippet "<Hi."}
        {snippet "Bye."}
</body></xml>
