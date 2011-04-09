fun bobo () =
    n1 <- source 0;
    n2 <- source 1;
    return <xml>
      <dyn signal={n1 <- signal n1; n2 <- signal n2; return <xml>{[n1 + n2]}</xml>}/>
      <button value="Increment1" onclick={v <- get n1; set n1 (v + 1)}/>
      <button value="Increment2" onclick={v <- get n2; set n2 (v + 1)}/>
    </xml>

fun main () =
    x <- source <xml/>;
    return <xml><body>
      <dyn signal={signal x}/>
      <button value="Grab" onclick={y <- rpc (bobo ()); set x y}/>
    </body></xml>
