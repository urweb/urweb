fun main () : transaction page =
  a <- source "";
  b <- source "";
  return <xml><body>
      <ctextbox source={a}/>
      <button onclick={fn _ => x <- get a; set b x}>click me</button>
      <div>
      <dyn signal={v <- signal a; return <xml>{[v]}</xml>}/>
      / <dyn signal={v <- signal b; return <xml>{[v]}</xml>}/>
      </div>
  </body></xml>
