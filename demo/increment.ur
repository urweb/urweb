sequence seq

fun increment () = nextval seq

fun main () =
    src <- source 0;
    return <xml><body>
      <dyn signal={n <- signal src; return <xml>{[n]}</xml>}/>
      <button value="Update" onclick={n <- rpc (increment ()); set src n}/>
    </body></xml>
