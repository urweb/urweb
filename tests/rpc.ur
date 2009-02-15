sequence s

fun main () : transaction page =
    let
        fun getNext () = nextval s
    in
        s <- source 0;
        return <xml><body>
          <button value="Get It On!"
                  onclick={n <- getNext ();
                           set s n}/><br/>
          <br/>
          Current: <dyn signal={n <- signal s; return <xml>{[n]}</xml>}/>
        </body></xml>
    end
