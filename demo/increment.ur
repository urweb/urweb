sequence seq

fun increment () = nextval seq

fun action () =
    src <- source 0;
    return <xml><body>
      <dyn signal={n <- signal src; return <xml>{[n]}</xml>}/>
      <button value="Update" onclick={n <- increment (); set src n}/>
    </body></xml>

fun main () = return <xml><body>
  <form><submit value="Begin demo" action={action}/></form>
</body></xml>
