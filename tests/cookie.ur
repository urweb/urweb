cookie c : string

fun other () =
    so <- getCookie c;
    case so of
        None => return <xml>No cookie</xml>
      | Some s => return <xml>Cookie: {[s]}</xml>

structure M = struct
    fun aux () =
        setCookie c "Hi";
        so <- getCookie c;
        case so of
            None => return <xml>No cookie</xml>
          | Some s => return <xml><body>Cookie: {[s]}<br/>
            <a link={other ()}>Other</a></body></xml>
end

fun main () : transaction page = return <xml><body>
  <a link={other ()}>Other</a><br/>
  <a link={M.aux ()}>Aux</a><br/>
</body></xml>
