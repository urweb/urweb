table channels : { Client : client, Channel : channel (string * int * float) }
  PRIMARY KEY Client

fun getInfo v =
    me <- self;
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Client <> {[me]});
    send r.Channels.Channel v

fun sendInfo v =
    me <- self;
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Client <> {[me]});
    send r.Channels.Channel v



fun clientOne () =
    me <- self;
    ch <- channel;
    dml (INSERT INTO channels (Client, Channel) VALUES ({[me]}, {[ch]}));

    buf <- Buffer.create;

    let
        fun receiver () =
            v <- recv ch;
            Buffer.write buf ("(" ^ v.1 ^ ", " ^ show v.2 ^ ", " ^ show v.3 ^ ")");
            rpc(sendInfo("Complete",2,4.0));
            receiver()


    in
     src <- source ("",1,4.0);
     return <xml><body onload={spawn (receiver())}>
       <h1>Client One </h1>
       <button value="GetInfo" onclick={fn _ => rpc(getInfo ("",2,4.0))}></button>
       <dyn signal={Buffer.render buf}/>
     </body></xml>
    end


fun clientTwo () =
    me <- self;
    ch <- channel;
    dml (INSERT INTO channels (Client, Channel) VALUES ({[me]}, {[ch]}));

    buf <- Buffer.create;

    let
        fun receiver () =
            v <- recv ch;
            Buffer.write buf ("(" ^ v.1 ^ ", " ^ show v.2 ^ ", " ^ show v.3 ^ ")");
            receiver()

    in
     src <- source ("",1,4.0);
     return <xml><body onload={spawn (receiver())}>
       <h1>Client Two </h1>
       <button value="GetInfo" onclick={fn _ => rpc(getInfo ("",1,4.0))}></button>
       <dyn signal={Buffer.render buf}/>
     </body></xml>
    end


fun main () = return <xml>
    <head>WebRTC</head>
    <body>
        <form><submit value="One" action={clientOne}/></form>
        <form><submit value="Two" action={clientTwo}/></form>
    </body>
    </xml>
