fun lister () = List.tabulateM (fn _ => n <- rand; return (n % 100)) 8

fun main () : transaction page =
    inp <- source [];
    return <xml><body>
      <button value="Compute" onclick={fn _ =>
                                          ls <- rpc (lister ());
                                          set inp ls}/>

      <dyn signal={inp <- signal inp; return (txt inp)}/>
      -&gt;
      <dyn signal={inp <- signal inp; return (txt (List.groupBy (fn n m => n % 2 = m % 2) inp))}/>
    </body></xml>
