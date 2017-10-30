table channels : { Client : client, Channel : channel (string * int * float) }
  PRIMARY KEY Client

fun getInfo v =
    me <- self;
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Client <> {[me]});
    send r.Channels.Channel v

fun sendInfo v =
    buf <- Buffer.create;
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
            Buffer.write buf ("(Received : " ^ v.1 ^ ")");
            Buffer.write buf ("(Sending : " ^ "I am good! Thank you for asking" ^ ")");
            rpc(sendInfo("I am good! Thank you for asking!",2,4.0));
            receiver()

        fun getFromOtherClient v =
            Buffer.write buf ("(Sending : " ^ v.1 ^ ")");
            rpc(getInfo v)


    in
     src <- source ("",1,4.0);
     return <xml><body onload={spawn (receiver())}>
       <h1>Client One </h1>
       <button value="GetInfo" onclick={fn _ => getFromOtherClient("How is Ur/Web Project?",1,4.1)}></button><br/>
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
            Buffer.write buf ("(Received :" ^ v.1 ^ ")");
            receiver()

        fun getFromOtherClient v =
            Buffer.write buf ("(Sending : " ^ v.1 ^ ")");
            rpc(getInfo v)

    in
     src <- source ("",1,4.0);
     return <xml><body onload={spawn (receiver())}>
       <h1>Client Two </h1>
       <button value="GetInfo" onclick={fn _ => getFromOtherClient("Hello! How are you?",1,4.1)}></button><br/>
       <dyn signal={Buffer.render buf}/>
     </body></xml>
    end


fun main () = return <xml>
    <head><title>WebRTC</title></head>
    <body>
        <form><submit value="One" action={clientOne}/></form>
        <form><submit value="Two" action={clientTwo}/></form>
        <button value="Click me please" onclick={fn _ => JsWebrtcJs.createOffer "123"; sleep 1000; x<- JsWebrtcJs.getDatastore "offer"; alert x}></button>
    </body>
    </xml>
