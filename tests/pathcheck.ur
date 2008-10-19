fun ancillary () : transaction page = return <xml/>

fun ancillary () = return <xml>
        Welcome to the ancillary page!
</xml>

fun main () : transaction page = return <xml><body>
        <a link={ancillary ()}>Enter the unknown!</a>
</body></xml>
