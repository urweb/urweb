fun handler r = return <xml><body>
  A: {[r.A]}<br/>
  B: {[r.B]}<br/>
  C: {[r.C]}<br/>
</body></xml>

fun main () =
    return <xml><body>
      <form>
        <textbox{#A} required placeholder="bobby"/>
        <textbox{#B} placeholder="soggy" autofocus/>
        <checkbox{#C}/>

        <submit action={handler}/>
      </form>
    </body></xml>
