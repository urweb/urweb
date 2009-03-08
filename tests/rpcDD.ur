datatype list t = Nil | OtherNil | Cons of t * list t

table t : {A : int}

fun main () : transaction page =
    let
        fun rows () =
            query (SELECT * FROM t)
            (fn r ls => return (Cons (r.T.A, ls)))
            Nil

        fun show ls =
            case ls of
                Nil => <xml/>
              | OtherNil => <xml>That's impossible!</xml>
              | Cons (x, ls') => <xml>{[x]}<br/>{show ls'}</xml>
    in
        s <- source Nil;
        return <xml><body>
          <button value="Get It On!"
                  onclick={ls <- rows ();
                           set s ls}/><br/>
          <br/>
          Current: <dyn signal={ls <- signal s; return (show ls)}/>
        </body></xml>
    end
