sequence s

fun main () : transaction page =
    let
        fun getNext () = nextval s
    in
        s <- source 0;
        return <xml><body>
          <button value="Get It On!"
                  onclick={n <- getNext ();
                           set s n}/>
        </body></xml>
    end
