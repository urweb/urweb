fun handler x = return <xml><body>
  You entered: {[case x.A of
                     None => "nothing at all"
                   | Some v => v]}
</body></xml>

fun main () = return <xml><body>
  <form>
    <radio{#A}>
      <li><radioOption value="A"/>A</li>
      <li><radioOption value="B"/>B</li>
    </radio>
    <submit action={handler}/>
  </form>
</body></xml>
