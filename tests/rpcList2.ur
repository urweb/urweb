fun rpcFunc l : transaction {} = return ()

fun main () : transaction page = return <xml><body>
  <button onclick={
    rpc (rpcFunc (("" :: []) :: []))
  }/>
  </body></xml>
