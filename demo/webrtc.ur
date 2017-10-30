table channels : { Client : client, Username: string, Channel : channel (int) }
  PRIMARY KEY Client

fun getInfo v =
    me <- self;
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Client <> {[me]});
    send r.Channels.Channel v

fun connectUser v =
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Username = {[v.1]});
    send r.Channels.Channel v.2


fun createChannel r =
    me <- self;
    ch <- channel;
    dml (INSERT INTO channels (Client, Username, Channel) VALUES ({[me]}, {[readError r.Username]}, {[ch]}));
    buf <- Buffer.create;
    src <- source 1;

    let 

        fun getFromOtherClient vl =  
            v <- get vl;
            Buffer.write buf ("(Sending : " ^ show v ^ ")");
            set src (v+1);
            rpc(getInfo v)

        fun pingUser vl =  
            Buffer.write buf ("(Sending : Hello " ^ show vl ^ ")");
            rpc(connectUser (vl,3))

        fun getActiveClients () =
            me <- self;
            list <- queryX (SELECT channels.Username FROM channels WHERE channels.Client <> {[me]})
                           (fn row => <xml><tr>
                                    <td>{[row.Channels.Username]}</td>
                                    <td>
                                    <button value="Connect" onclick={fn _ => pingUser (row.Channels.Username)}/>
                                </td></tr></xml>);
            return list

        fun receiver () =
            v <- recv ch;
            Buffer.write buf ("(Received : " ^ show v ^ ")");
            if v % 2 <> 0 then 
                Buffer.write buf ("(Sending : " ^ show (v+1) ^ ")");
                set src (v+2);
                rpc(getInfo(v+1));
                receiver()
            else
                set src (v+1);
                Buffer.write buf ("Completed one round trip");
            receiver()

        


    in
        activeClients <- getActiveClients();     
        return <xml><body onload={spawn (receiver())}>
            <h1>Hello WebRTC </h1>
            <table>
                <tr>
                    <th>Username</th>
                    <th>Action</th>
                </tr>
                {activeClients}
            </table>
           <button value="Ping" onclick={fn _ => getFromOtherClient(src)}></button><br/>
           <dyn signal={Buffer.render buf}/>
        </body></xml>
    end


fun main () = return <xml>
    <head><title>WebRTC</title></head>
    <body>
        <form>
            <textbox{#Username}/>
            <submit value="One" action={createChannel}/>
        </form>
        <button value="Click me please" onclick={fn _ => n<- JsWebrtcJs.myFunction "Nitin Surana"; alert n}></button>
    </body>
    </xml>
