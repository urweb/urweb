fun main () : transaction page =
  a <- source "";
  b <- source "";
  return <xml><body>
    <form>
      <textbox{#A} source={a}/>
      <button onclick={x <- get a; set b x}/>
      <dyn signal={v <- signal a; return <xml>{[v]}</xml>}/>
      / <dyn signal={v <- signal b; return <xml>{[v]}</xml>}/>
    </form>
  </body></xml>
