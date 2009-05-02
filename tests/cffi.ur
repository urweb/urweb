fun effect () =
  Test.print;
  return <xml/>

fun main () = return <xml><body>
  {[Test.out (Test.frob (Test.create "Hello ") "world!")]}
  <form><submit action={effect}/></form>
</body></xml>
