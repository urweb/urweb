fun printer () = Test.foo

fun effect () =
  Test.print;
  return <xml><body>
    <button value="Remote" onclick={printer ()}/>
    <button value="Local" onclick={Test.bar "Hoho"}/>
    <button value="Either" onclick={Test.print}/>
  </body></xml>

fun main () = return <xml><body>
  {[Test.out (Test.frob (Test.create "Hello ") "world!")]}
  <form><submit action={effect}/></form>
</body></xml>
