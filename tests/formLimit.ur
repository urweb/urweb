fun handler r = return <xml><body>
  {[r.A]}, {[r.B]}
</body></xml>

fun main () = return <xml><body>
  <form>
    <textbox{#A}/>
    <textbox{#B}/>
    <submit action={handler}/>
  </form>
</body></xml>
