fun main () : transaction page =
    ua <- requestHeader "UserAgent";
    case ua of
        None => return <xml>Not found</xml>
      | Some s => return <xml>UserAgent: {[s]}</xml>
