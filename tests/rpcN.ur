table t : { A : int }

fun main () : transaction page =
    let
        fun count a = r <- oneRow (SELECT COUNT( * ) AS N FROM t WHERE t.A = {[a]});
                      return r.N
    in
        s <- source 0;
        return <xml><body>
          <button value="Get It On!"
                  onclick={n <- count 3;
                           set s n}/><br/>
          <br/>
          Current: <dyn signal={n <- signal s; return <xml>{[n]}</xml>}/>
        </body></xml>
    end
