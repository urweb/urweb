fun main () : transaction page =
    x <- fresh;
    s <- source 0;
    q <- source "";
    return <xml><body>
      <span id={x}/>
      <button onclick={v <- get q; set q (v ^ "!"); Ffi.setIt x <xml><dyn signal={n <- signal s; return <xml>n = {[n]}</xml>}/>{[v]}</xml>}/>
      <button onclick={n <- get s; set s (n + 1)}/>
    </body></xml>
