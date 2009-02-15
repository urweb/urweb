sequence s
sequence s2

fun dint src = n <- signal src; return <xml>{[n]}</xml>

fun main () : transaction page =
    let
        fun getNext () =
            n <- nextval s;
            n2 <- nextval s2;
            return (n, n2)
    in
        src1 <- source 0;
        src2 <- source 0;
        return <xml><body>
          <button value="Get It On!"
                  onclick={p <- getNext ();
                           case p of
                               (n1, n2) => set src1 n1;
                                           set src2 n2}/>
          <br/>
          Current1: <dyn signal={dint src1}/>
          Current2: <dyn signal={dint src2}/>
        </body></xml>
    end
