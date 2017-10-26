table channels : { Client : client, Channel : channel (int) }
  PRIMARY KEY Client

fun getInfo v =
    me <- self;
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Client <> {[me]});
    send r.Channels.Channel v


fun clientOne () =
    me <- self;
    ch <- channel;
    dml (INSERT INTO channels (Client, Channel) VALUES ({[me]}, {[ch]}));
    buf <- Buffer.create;
    src <- source 1;

    let 

        fun receiver () =
            v <- recv ch;
            Buffer.write buf ("(Received : " ^ show v ^ ")");
            if v % 2 <> 0 then 
                Buffer.write buf ("(Sending : " ^ show (v+1) ^ ")");
                set src (v+2);
                rpc(getInfo(v+1))
            else
                set src (v+1);
                Buffer.write buf ("Completed one round trip");
            receiver()

        fun getFromOtherClient vl =  
            v <- get vl;
            Buffer.write buf ("(Sending : " ^ show v ^ ")");
            set src (v+1);
            rpc(getInfo v)


    in     
     return <xml><body onload={spawn (receiver())}>
       <h1>Hello WebRTC </h1>
       <button value="Ping" onclick={fn _ => getFromOtherClient(src)}></button><br/>
       <dyn signal={Buffer.render buf}/>
     </body></xml>
    end


fun main () = return <xml>
    <head><title>WebRTC</title></head>
    <body>
        <form><submit value="One" action={clientOne}/></form>
        <button value="Click me please" onclick={fn _ => n<- JsWebrtcJs.myFunction "Nitin Surana"; alert n}></button>
    </body>
    </xml>
