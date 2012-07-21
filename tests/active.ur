fun counter' () =
    s <- source 0;
    return <xml>
      <dyn signal={n <- signal s; return (txt n)}/>
      <button onclick={fn _ => n <- get s; set s (n + 1)}/>
    </xml>

fun counter () = <xml><active code={counter' ()}/></xml>

fun main () : transaction page = return <xml><body>
  {counter ()}
  <hr/>
  {counter ()}
</body></xml>
