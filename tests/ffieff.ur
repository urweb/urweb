open Ffi_eff

fun main () : transaction page = return <xml><body>
  <button value="shout" onclick={fn _ => shout "Hi!"; shout "Bye!"}/>
  <button value="sneakyShout" onclick={fn _ => let val x = sneakyShout "Whoa" in return () end}/>
</body></xml>
