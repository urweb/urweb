fun main () =
  s <- source "You didn't click it yet.";
  return <xml><body>
    <button value="Click me!" onclick={fn _ => set s "Now you clicked it."}/><br/>
    <dyn signal={v <- signal s; return <xml>{[v]}</xml>}/>
  </body></xml>
