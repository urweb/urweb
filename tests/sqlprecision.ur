table t : { N : float }

fun insert r =
    dml (INSERT INTO t (N) VALUES ({[readError r.N]}));
    return <xml/>

fun main () = return <xml><body>
  <form>
    <textbox{#N}/>
    <submit action={insert}/>
  </form>
</body></xml>
