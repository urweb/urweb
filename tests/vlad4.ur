fun main () : transaction page =
    s <- source "";
    return <xml><body>
      <ctextbox source={s} value="123" onchange={s <- get s; alert (s ^ "!")}/>
      <dyn signal={s <- signal s; return (txt s)}/>
    </body></xml>
