fun cryptIt r = return <xml><body>
  {[crypt r.Pass "AB"]}
</body></xml>

fun main () = return <xml><body>
  <form><textbox{#Pass}/> <submit action={cryptIt}/></form>
</body></xml>
