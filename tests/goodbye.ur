table boo : { Client : client, Channel : channel unit }

fun doIt () =
    me <- self;
    ch <- channel;
    dml (INSERT INTO boo (Client, Channel) VALUES ({[me]}, {[ch]}));
    return <xml><body onload={let
                                  fun loop () =
                                      v <- recv ch;
                                      alert "Someone left";
                                      loop ()
                              in
                                  loop ()
                              end}/></xml>

task clientLeaves = fn cl =>
                       debug "Client left";
                       dml (DELETE FROM boo WHERE Client = {[cl]});
                       queryI (SELECT (boo.Channel)
                               FROM boo)
                       (fn r => send r.1 ());
                       debug "Done processing"

fun main () = return <xml><body>
  <form> <submit action={doIt}/> </form>
</body></xml>
