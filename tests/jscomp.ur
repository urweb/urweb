fun fst [a] [b] (x : a) (y : b) = x
fun snd [a] [b] (x : a) (y : b) = y

fun main () =
    s <- source "";
    s' <- source "";
    f <- source (plus 1);
    f2 <- source fst;
    r <- source {A = "x", B = "y"};

    return <xml><body>
      <ctextbox source={s}/> <ctextbox source={s'}/><br/><br/>

      Function: <button value="+1" onclick={set f (plus 1)}/>
      <button value="*3" onclick={set f (times 3)}/><br/><br/>

      Function2: <button value="Fst" onclick={set f2 fst}/>
      <button value="Snd" onclick={set f2 snd}/><br/><br/>

      <button value="Echo" onclick={s <- get s; alert s}/>
      <button value="-" onclick={s <- get s; alert (show (-(readError s : int)))}/>
      <button value="+1" onclick={s <- get s; alert (show (readError s + 1))}/>
      <button value="*3" onclick={s <- get s; alert (show ((readError s) * 3))}/>
      <button value="f" onclick={s <- get s; f <- get f; alert (show (f (readError s)))}/><br/><br/>

      <button value="f2" onclick={s <- get s; s' <- get s'; f2 <- get f2; alert (f2 s s')}/><br/><br/>

      <button value="A" onclick={r <- get r; alert r.A}/>
      <button value="B" onclick={r <- get r; alert r.B}/>
    </body></xml>
