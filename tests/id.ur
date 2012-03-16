fun main () : transaction page =
    id1 <- fresh;
    id2 <- fresh;
    x <- source <xml/>;
    return <xml><body>
      <span id={id1}>Hi!</span>
      <span id={id2}>Ho!</span>
      <dyn signal={signal x}/>
      <button value="Set" onclick={id <- fresh; set x <xml><span id={id}>He!</span></xml>}/>
      <button value="Show" onclick={x <- get x; alert (show x)}/>
    </body></xml>
