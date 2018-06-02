fun main () : transaction page = return <xml><body>
  <active code={s <- source ""; return <xml>
    <dyn signal={s <- signal s; return (txt s)}/>
    <button onclick={fn _ => set s "Hi!"}>Click me!</button>
  </xml>}/>

  <active code={sleep 1; return <xml>Hi!</xml>}/>

  <active code={spawn (sleep 1; alert "Hi!"); return <xml>Success</xml>}/>
</body></xml>
