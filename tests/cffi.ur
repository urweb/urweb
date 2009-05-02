fun printer () = Test.foo

fun effect () =
  Test.print;
  return <xml><body>
    <button value="Remote" onclick={printer ()}/>
    <button value="Local" onclick={Test.bar "Hoho"}/>
    <button value="Either" onclick={Test.print}/>
  </body></xml>

fun xact () =
  Test.transactional;
  return <xml><body>
    All good.
  </body></xml>

fun xact2 () =
  Test.transactional;
  error <xml>Failure</xml>;
  return <xml><body>
    All gooder.
  </body></xml>

fun main () = return <xml><body>
  {[Test.out (Test.frob (Test.create "Hello ") "world!")]}
  <form><submit action={effect}/></form>
  <form><submit action={xact}/></form>
  <form><submit action={xact2}/></form>
</body></xml>
