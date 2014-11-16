fun handler r = return <xml><body>
  A: {[r.A]}<br/>
  B: {[r.B]}<br/>
  C: {[r.C]}<br/>
  D: {[r.D]}<br/>
  E: {[r.E]}<br/>
  F: {[r.F]}<br/>
  G: {[r.G]}<br/>
  H: {[r.H]}<br/>
  I: {[r.I]}<br/>
  J: {[r.J]}<br/>
  K: {[r.K]}<br/>
  L: {[r.L]}<br/>
  M: {[r.M]}<br/>
  N: {[r.N]}<br/>
  O: {[r.O]}<br/>
  P: {[r.P]}<br/>
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

        <hr/>

        <color{#H}/>
        <number{#I} min={17.0} max={32.8} value={20.6} step={2.5}/>
        <range{#J} min={17.0} max={32.8} value={20.6}/>
        <date{#K}/>
        <datetime{#L}/>
        <datetime-local{#M}/>
        <month{#N}/>
        <week{#O}/>
        <timeInput{#P}/>

        <submit action={handler}/>
      </form>
    </body></xml>
