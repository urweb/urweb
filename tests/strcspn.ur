fun main () : transaction page =
    s <- source "";
    return <xml><body>
      <ctextbox source={s}/>
      <button onclick={fn _ => v <- get s; alert (show (strcspn v "0123456789"))}/>
    </body></xml>
