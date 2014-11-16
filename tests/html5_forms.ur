fun handler r = return <xml><body>
  A: {[r.A]}<br/>
  B: {[r.B]}<br/>
  C: {[r.C]}<br/>
  D: {[r.D]}<br/>
  E: {[r.E]}<br/>
  F: {[r.F]}<br/>
  G: {[r.G]}<br/>
</body></xml>

fun main () =
    return <xml><body>
      <form>
        <textbox{#A} required placeholder="bobby"/>
        <textbox{#B} placeholder="soggy" autofocus/>
        <checkbox{#C}/>
        <email{#D}/>
        <url{#E}/>
        <tel{#F}/>
        <search{#G}/>

        <submit action={handler}/>
      </form>
    </body></xml>
