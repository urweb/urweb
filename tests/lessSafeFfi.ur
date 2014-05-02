ffi foo : int -> int
ffi bar serverOnly benignEffectful : int -> transaction unit
ffi baz : transaction int

ffi bup jsFunc "jsbup" : int -> transaction unit

fun other () : transaction page =
    (*bar 17;
    q <- baz;*)
    return <xml><body>
      (*{[foo 42]}, {[q]}*)
      <button onclick={fn _ => bup 32}/>
    </body></xml>

fun main () = return <xml><body>
  <form>
    <submit action={other}/>
  </form>
</body></xml>
