cookie c : string

fun main () : transaction page =
    setCookie c "Hi";
    so <- getCookie c;
    case so of
        None => return <xml>No cookie</xml>
      | Some s => return <xml>Cookie: {[s]}</xml>

