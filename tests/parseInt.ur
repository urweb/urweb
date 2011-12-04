fun main () : transaction page =
    s <- source "";
    return <xml><body>
      <ctextbox source={s}/>
      <button onclick={n <- get s;
                       case read n of
                           None => alert "Invalid"
                         | Some n => alert (show (n + 1))}/>
    </body></xml>
