fun handler'' ls =
    case ls of
        Nil => <xml/>
      | Cons (r, ls) => <xml><li>{[r.C]}</li>{handler'' ls}</xml>

fun handler' ls =
    case ls of
        Nil => <xml/>
      | Cons (r, ls) => <xml><li>{[r.Sub.A]} <ul>{handler'' r.Sub.Sub2}</ul></li>{handler' ls}</xml>

fun handler r = return <xml><body>
  {[r.A]}
  <ul>{handler' r.Sub}</ul>
  {[r.C]}<br/>
  {[r.Sub2.A]}<br/>
  {handler'' r.Sub2.Nested}
</body></xml>

fun main () = return <xml><body>
  <form>
    <textbox{#A}/><br/>
    <subforms{#Sub}>
      <entry>
        <subform{#Sub}>
          <textbox{#A}/><br/>
          <subforms{#Sub2}>
            <entry>
              <textbox{#C}/><br/>
            </entry>

            <entry>
              <textbox{#C}/><br/>
            </entry>
          </subforms>
        </subform>
      </entry>

      <entry>
        <subform{#Sub}>
          <textbox{#A}/><br/>
          <subforms{#Sub2}>
            <entry>
              <textbox{#C}/><br/>
            </entry>

            <entry>
              <textbox{#C}/><br/>
            </entry>
          </subforms>
        </subform>
      </entry>
    </subforms>
    <textbox{#C}/><br/>

    <subform{#Sub2}>
      <textbox{#A}/><br/>

      <subforms{#Nested}>
        <entry>
          <textbox{#C}/>
        </entry>
      </subforms>
    </subform><br/>

    <submit action={handler}/>
  </form>
</body></xml>
