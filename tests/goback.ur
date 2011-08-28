table channels : { Channel : channel {} }

fun get () =
    ch <- channel;
    dml (INSERT INTO channels (Channel) VALUES ({[ch]}));
    return <xml><body onload={recv ch}>
      Hi.
    </body></xml>

fun post () =
    ch <- channel;
    dml (INSERT INTO channels (Channel) VALUES ({[ch]}));
    return <xml><body onload={recv ch}>
      Hi!
    </body></xml>

fun main () = return <xml><body>
  <li><a link={get ()}>Get</a></li>
  <li><form><submit action={post}/></form></li>
</body></xml>
