structure Globals :> GLOBALS = struct

val resetTime = ref (Time.zeroTime)
fun setResetTime () = resetTime := Time.now ()
fun getResetTime () = !resetTime

end
