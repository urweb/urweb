fun main () =
    s <- source "";
    f <- source (plus 1); 

    return <xml><body>
      <ctextbox source={s}/><br/><br/>

      Function: <button value="+1" onclick={set f (plus 1)}/>
      <button value="*3" onclick={set f (times 3)}/><br/><br/>

      <button value="Echo" onclick={s <- get s; alert s}/>
      <button value="+1" onclick={s <- get s; alert (show (readError s + 1))}/>
      <button value="*3" onclick={s <- get s; alert (show ((readError s) * 3))}/>
      <button value="f" onclick={s <- get s; f <- get f; alert (show (f (readError s)))}/>
    </body></xml>
