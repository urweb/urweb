fun handler' ls =
    case ls of
        Nil => <xml/>
      | Cons (r, ls) => <xml><li>{[r.Nam]}, {[r.A]}, {[r.B]}, {[r.Sub]}</li>{handler' ls}</xml>

fun handler r = return <xml><body>
  {[r.A]}<br/>
  {handler' r.Sub}
  {[r.C]}
</body></xml>

fun main () = return <xml><body>
  <form>
    <textbox{#A}/><br/>
    <subforms{#Sub}>
      <entry>
        <hidden{#Nam} value="Sparky"/>
        <textbox{#A}/><br/>
        <textbox{#B}/><br/>
        <textbox{#Sub}/><br/>
      </entry>

      <entry>
        <hidden{#Nam} value="Snarky"/>
        <textbox{#A}/><br/>
        <textbox{#B}/><br/>
        <textbox{#Sub}/><br/>
      </entry>
    </subforms>
    <textbox{#C}/><br/>
    <submit action={handler}/>
  </form>
</body></xml>
