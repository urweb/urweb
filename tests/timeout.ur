table listeners : { Ch : channel unit }

fun ping () =
    queryI1 (SELECT * FROM listeners)
    (fn r => send r.Ch ())

fun main () =
    ch <- channel;
    dml (INSERT INTO listeners(Ch) VALUES ({[ch]}));
    count <- source 0;
    return <xml><body onload={let
                                  fun loop () =
                                      _ <- recv ch;
                                      c <- get count;
                                      set count (c + 1);
                                      loop ()
                              in
                                  loop ()
                              end}>
      <dyn signal={n <- signal count; return (txt n)}/>
      <button onclick={fn _ => rpc (ping ())}>Ping</button>
    </body></xml>
