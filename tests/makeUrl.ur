fun other () = return <xml>Hi!</xml>

fun main () = return <xml>{[Basis.url (main ())]}, {[url (other ())]}</xml>
