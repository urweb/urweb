fun main () : transaction page =
    i <- fresh;
    return <xml><body>
      <ctextbox/>
      <ctextbox id={i}/>
      <active code={giveFocus i; return <xml>Done</xml>}/>
    </body></xml>

fun dynamic () : transaction page =
    x <- source <xml/>;
    return <xml><body>
      <dyn signal={signal x}/>
      <button onclick={fn _ => i <- fresh; set x <xml>
        <ctextbox/>
        <ctextbox id={i}/>
        <active code={giveFocus i; return <xml>Done</xml>}/>
      </xml>}>Click</button>
    </body></xml>
