table channels : { Client : client, Username: string, Channel : channel (string * string) }
  PRIMARY KEY Client

fun connectUser v =
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Username = {[v.1]});
    send r.Channels.Channel (v.3, v.2)

fun createChannel r =
    me <- self;
    ch <- channel;
    dml (INSERT INTO channels (Client, Username, Channel) VALUES ({[me]}, {[readError r.Username]}, {[ch]}));
    buf <- Buffer.create;
    src <- source 1;
    user <- source r.Username;

    let 

        fun pingUser vl =
            Buffer.write buf ("(Sending to " ^ show vl.1 ^ " : Ask )");
            rpc(connectUser (vl.1,"Ask", vl.2))

        fun getActiveClients () =
            me <- self;
            list <- queryX (SELECT channels.Username FROM channels WHERE channels.Client <> {[me]})
                           (fn row => <xml><tr>
                                    <td>{[row.Channels.Username]}</td>
                                    <td>
                                    <button value="Connect" onclick={fn _ => pingUser (row.Channels.Username, r.Username)}/>
                                </td></tr></xml>);
            return list

        fun receiver () =
            v <- recv ch;
            username <- get user;
            Buffer.write buf ("(Received from " ^ v.1 ^" : " ^ show v.2 ^ ")");

            if v.2 <> "Reply" then 
                Buffer.write buf ("(Sending to " ^ show v.1 ^ ": Reply)");
                rpc( connectUser (v.1 , "Reply", username) );
                receiver()
            else
                Buffer.write buf ("Completed one round trip");
            receiver()

        


    in
        activeClients <- getActiveClients();     
        return <xml><body onload={spawn (receiver())}>
            <dyn signal={v <- signal user; return <xml><h1>Hello {[v]} </h1></xml>}/>
            <h2>List of Active Clients</h2>
            <table border=1>
                <tr>
                    <th>Username</th>
                    <th>Action</th>
                </tr>
                {activeClients}
            </table>
            <h3>Messaging Snapshot</h3>
           <dyn signal={Buffer.render buf}/>
        </body></xml>
    end


fun main () =
let
    fun urWebFromDatastore v =
        sleep 1000;
        x <- JsWebrtcJs.getDatastore v;
        if x = "undefined" then
            urWebFromDatastore v
        else
            return x


    fun eventHandler() =
        sleep 1000;
        x <- JsWebrtcJs.getPendingEvent();
        if x = "undefined" then
            eventHandler()
        else
            alert x;
            JsWebrtcJs.clearPendingEvent();
            eventHandler()

in
return <xml>
    <head><title>WebRTC</title></head>
    <body onload={spawn (eventHandler())}>
        <form>
            <textbox{#Username}/>
            <submit value="Create Channel" action={createChannel}/>
        </form>
        <button value="Click me please" onclick={fn _ => n <- JsWebrtcJs.myFunction "Nitin Surana Test"; n<- urWebFromDatastore "key"; alert n}></button>
    </body>
    </xml>
end