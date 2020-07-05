table active : {
      Client : client,
      Channel : channel unit,
} PRIMARY KEY (Client)

val refresh : transaction unit =
    queryI (SELECT active.Channel FROM active)
	   (fn r => send r.Active.Channel ())

task periodic 5 =
  fn _ => refresh

fun loop m = m; loop m

val main : transaction page =
    me <- self;
    ch <- channel;
    dml (INSERT INTO active(Client, Channel) VALUES ({[me]}, {[ch]}));
    refresh;

    return <xml>
      <body onload={spawn (loop (recv ch))}>
	hello
      </body>
    </xml>
