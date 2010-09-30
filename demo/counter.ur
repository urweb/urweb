(* Workhorse function, which [main] will call *)
fun counter n = return <xml><body>
  Current counter: {[n]}<br/>
  <a link={counter (n + 1)}>Increment</a><br/>
  <a link={counter (n - 1)}>Decrement</a>
</body></xml>

fun main () = counter 0
