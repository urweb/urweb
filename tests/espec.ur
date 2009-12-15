fun foo (wrap : xbody -> transaction page) = wrap <xml>
  <a link={foo wrap}>Foo</a>
</xml>

fun bar (wrap : xbody -> transaction page) (n : int) = wrap <xml>
  <a link={bar wrap n}>Bar</a>; {[n]}
</xml>

fun baz (n : int) (wrap : xbody -> transaction page) = wrap <xml>
  <a link={baz n wrap}>Baz</a>; {[n]}
</xml>

fun middle (n : int) (wrap : xbody -> transaction page) (m : int) = wrap <xml>
  <a link={middle n wrap m}>Middle</a>; {[n]}; {[m]}
</xml>

fun crazy (f : int -> int) (b : bool) (wrap : xbody -> transaction page) (m : int) = wrap <xml>
  <a link={crazy f b wrap m}>Crazy</a>; {[b]}; {[f m]}
</xml>

fun wild (q : bool) (f : int -> int) (n : float) (wrap : xbody -> transaction page) (m : int) = wrap <xml>
  <a link={wild q f n wrap m}>Wild</a>; {[n]}; {[f m]}; {[q]}
</xml>

fun wrap x = return <xml><body>{x}</body></xml>

fun wrapN n x = return <xml><body>{[n]}; {x}</body></xml>

fun foo2 (wrap : xbody -> transaction page) = wrap <xml>
  <a link={foo2 wrap}>Foo</a>
</xml>

fun foo3 (n : int) = wrap <xml>
  <a link={foo2 (wrapN n)}>Foo</a>
</xml>

fun bar2 (n : int) (wrap : xbody -> transaction page) = wrap <xml>
  <a link={bar2 n wrap}>Bar</a>; n={[n]}
</xml>

fun bar3 (n : int) = wrap <xml>
  <a link={bar2 88 (wrapN n)}>Bar</a>
</xml>


fun main () = return <xml><body>
  <a link={foo wrap}>Foo</a>
  <a link={bar wrap 32}>Bar</a>
  <a link={baz 18 wrap}>Baz</a>
  <a link={middle 1 wrap 2}>Middle</a>
  <a link={crazy (fn n => 2 * n) False wrap 2}>Crazy</a>
  <a link={wild True (fn n => 2 * n) 1.23 wrap 2}>Wild</a>
  <hr/>
  <a link={foo3 15}>Foo3</a>
  <a link={bar3 44}>Bar3</a>
</body></xml>
