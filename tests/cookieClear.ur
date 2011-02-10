cookie c : int

fun setit () =
    setCookie c {Value = 13,
                 Expires = None,
                 Secure = False};
    return <xml/>

fun doit () =
    ro <- getCookie c;
    clearCookie c;
    case ro of
      None => return <xml>None</xml>
    | Some v => return <xml>Some {[v]}</xml>

fun main () = return <xml><body>
  <form><submit value="Set it!" action={setit}/></form>
  <form><submit value="Get busy!" action={doit}/></form>
</body></xml>
