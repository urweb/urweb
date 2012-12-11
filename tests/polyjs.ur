open PolyjsFfi

fun main () : transaction page = return <xml><body>
  <button onclick={fn _ => alert (one "hi" ^ two "bye")}/>
</body></xml>
