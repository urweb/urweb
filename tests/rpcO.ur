table t : {A : int}

fun main () : transaction page =
    let
        fun check () =
            r <- oneRow (SELECT SUM(t.A) AS X FROM t);
            return (if r.X < 0 then
                        (Some 3, None)
                    else
                        (None, Some "Hi"))

        fun show (t ::: Type) (_ : show t) (opt : option t) =
            case opt of
                None => <xml>None</xml>
              | Some v => <xml>{[v]}</xml>
    in
        s <- source (None, None);
        return <xml><body>
          <button value="Get It On!"
                  onclick={r <- check ();
                           set s r}/><br/>
          <br/>
          Current: <dyn signal={p <- signal s; return <xml>{show p.1}, {show p.2}</xml>}/>
        </body></xml>
    end
