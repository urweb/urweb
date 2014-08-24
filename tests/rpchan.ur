fun remote () =
    ch <- channel;
    send ch "Hello World!";
    return ch

fun remoter () =
    ch <- channel;
    send ch "Hello World!";
    return <xml><active code={spawn (s <- recv ch; alert s); return <xml/>}/></xml>

fun main () =
    x <- source <xml/>;
    return <xml><body>
      <button onclick={fn _ => ch <- rpc (remote ()); s <- recv ch; alert s}>TEST</button>
      <button onclick={fn _ => y <- rpc (remoter ()); set x y}>TESTER</button>
      <hr/>
      <dyn signal={signal x}/>
    </body></xml>
