fun id n = if n = 0 then 0 else 1 + id (n - 1)

fun main () : transaction page = return <xml><body>
  <button onclick={fn _ => alert (show (id 3))}/>
</body></xml>
