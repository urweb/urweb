cookie c : string

fun main () : transaction page =
    setCookie c "Hi";
    so <- requestHeader "Cookie";
    case so of
        None => return <xml>No cookie</xml>
      | Some s => return <xml>Cookie: {[s]}</xml>
