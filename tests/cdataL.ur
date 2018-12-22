val subpage : string -> transaction page = fn s => return <xml><body>
        <h1>{cdata s}</h1>
</body></xml>

val main : unit -> transaction page = fn () => return <xml><body>
        <li> <a link={subpage "<Hi."}>Door #1</a></li>
        <li> <a link={subpage "Bye."}>Door #2</a></li>
</body></xml>
