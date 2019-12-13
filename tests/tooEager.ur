fun test (i: list int) : transaction unit =
    a <- return (Some "abc");
    c <- (case a of
            None => return "1"
          | Some b =>
            debug "not happening :(";
            return "2"
         );
    (case i of
       [] => return ()
     | first :: _ => debug c)

fun main (): transaction page =
    return <xml>
      <body>
        <button onclick={fn _ => rpc (test [])}>click</button>
      </body>
    </xml>
