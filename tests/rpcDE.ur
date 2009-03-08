datatype result = Neg | Zero | Pos

table t : {A : int}

fun main () : transaction page =
    let
        fun check () =
            r <- oneRow (SELECT SUM(t.A) AS X FROM t);
            return (if r.X < 0 then
                        Neg
                    else if r.X = 0 then
                        Zero
                    else
                        Pos)

        fun show r =
            case r of
                Neg => <xml>-</xml>
              | Zero => <xml>0</xml>
              | Pos => <xml>+</xml>
    in
        s <- source Zero;
        return <xml><body>
          <button value="Get It On!"
                  onclick={r <- check ();
                           set s r}/><br/>
          <br/>
          Current: <dyn signal={r <- signal s; return (show r)}/>
        </body></xml>
    end
