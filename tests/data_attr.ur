fun dynd r = return <xml><body>
  <div data={data_attr data_kind r.Attr r.Value}>How about that?</div>
</body></xml>

fun main () : transaction page =
  s <- source <xml/>;
  a <- source "";
  v <- source "";
  return <xml><body>
    <div data-foo="hi" aria-something="wow" data-bar="bye" data-baz="why">Whoa there, cowboy!</div>

    <hr/>

    <form>
      <textbox{#Attr}/> = <textbox{#Value}/>
      <submit action={dynd}/>
    </form>

    <hr/>

    <ctextbox source={a}/> = <ctextbox source={v}/>
    <button onclick={fn _ =>
      a <- get a; v <- get v; set s <xml><div data={data_attr data_kind a v}>OHO!</div></xml>}/>
    <hr/>
    <dyn signal={signal s}/>
  </body></xml>
