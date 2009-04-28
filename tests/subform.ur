fun handler r = return <xml><body>
  {[r.A]}, {[r.Sub.A]}, {[r.Sub.B]}, {[r.Sub.Sub]}, {[r.C]}
</body></xml>

fun main () = return <xml><body>
  <form>
    <textbox{#A}/><br/>
    <subform{#Sub}>
      <textbox{#A}/><br/>
      <textbox{#B}/><br/>
      <textbox{#Sub}/><br/>
    </subform>
    <textbox{#C}/><br/>
    <submit action={handler}/>
  </form>
</body></xml>
