fun main () =
    s <- source "";

    return <xml><body>
      <ctextbox source={s}/><br/>

      <button value="Echo" onclick={s <- get s; alert s}/>
      <button value="+1" onclick={s <- get s; alert (show (readError s + 1))}/>
      <button value="*3" onclick={s <- get s; alert (show ((readError s) * 3))}/>
    </body></xml>
