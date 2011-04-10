table ms : {Client : client, Channel : channel xbody}

val hitMe =
    me <- self;
    ch <- oneRowE1 (SELECT (ms.Channel)
                    FROM ms
                    WHERE ms.Client = {[me]});

    s <- source 0;
    send ch <xml>
      <dyn signal={n <- signal s; return <xml>{[n]}</xml>}/>
      <button value="Inc" onclick={n <- get s; set s (n + 1)}/>
    </xml>

fun main' () =
    me <- self;
    ch <- channel;
    dml (INSERT INTO ms (Client, Channel) VALUES ({[me]}, {[ch]}));

    s <- source <xml/>;

    return <xml><body onload={let
                                  fun loop () =
                                      x <- recv ch;
                                      set s x;
                                      loop ()
                              in
                                  loop ()
                              end}>
      <button value="Another" onclick={rpc hitMe}/>
      <dyn signal={signal s}/>
    </body></xml>

fun main () =
    return <xml><body><form><submit action={main'}/></form></body></xml>
