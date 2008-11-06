fun main () : transaction page =
    ua <- requestHeader "User-Agent";
    case ua of
        None => return <xml>Not found</xml>
      | Some s => return <xml>User-Agent: {[s]}</xml>
