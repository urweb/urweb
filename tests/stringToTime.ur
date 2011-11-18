fun main () : transaction page =
    s <- source "";
    return <xml><body>
      <ctextbox source={s}/>
      <button onclick={v <- get s; alert (show (readError v : time))}/>
      <button onclick={v <- get s; case read v : option time of
                                       None => alert "Invalid"
                                     | Some tm => alert (show tm)}/>
    </body></xml>
