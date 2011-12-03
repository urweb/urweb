fun main () : transaction page =
    n <- source 0;
    return <xml><body>
      <button onclick={n' <- get n; set n (n' + 1); debug ("Message: " ^ show n')}/>
    </body></xml>
