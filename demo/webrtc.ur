datatype list t = Nil | Cons of t * list t

table channels : { Client : client, Username: string, Channel : channel (string * string) }
  PRIMARY KEY Client

style channelBox
style heading
style activeClientsTable

fun connectUser v =
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Username = {[v.1]});
    send r.Channels.Channel (v.3, v.2)

fun allRows (uname) =
    query (SELECT channels.Username FROM channels WHERE channels.Username <> {[uname]})
    (fn r acc => return (Cons ((r.Channels.Username), acc)))
    Nil

fun createChannel r =
    me <- self;
    ch <- channel;
    dml (INSERT INTO channels (Client, Username, Channel) VALUES ({[me]}, {[readError r.Username]}, {[ch]}));
    buf <- Buffer.create;
    src <- source 1;
    user <- source r.Username;
    lss <- source Nil;
    msg <- source "No messages so far!";

    let 

        fun pingUser vl =
            set msg "";
            Buffer.write buf ("(Sending to " ^ show vl.1 ^ " : Ask )");
            rpc(connectUser (vl.1,"Ask", vl.2))

        fun receiver () =
            v <- recv ch;
            username <- get user;
            set msg "";
            Buffer.write buf ("(Received from " ^ v.1 ^" : " ^ show v.2 ^ ")");

            if v.2 <> "Reply" then 
                Buffer.write buf ("(Sending to " ^ show v.1 ^ ": Reply)");
                rpc( connectUser (v.1 , "Reply", username) );
                receiver()
            else
                Buffer.write buf ("Completed one round trip");
            receiver()

        fun dynTable xyz =
            let
                fun disp v =
                    case v of 
                     Nil => <xml/>
                    | Cons ((uname), ls) => 
                        <xml>
                            <tr>
                                <td>{[uname]}</td>
                                <td>
                                    <button value="Connect" onclick={fn _ => pingUser (uname, r.Username)}/>
                                </td>
                            </tr>
                            {disp ls}
                        </xml>
            in
                 <xml><dyn signal={ls <- signal xyz; return <xml>
                 <table border=1 class={activeClientsTable}>
                     <tr>
                           <th>Username</th>
                           <th>Action</th>
                       </tr> 
                     {disp ls}
                </table>
                </xml>}/></xml>
            end

    in
        clientList <- allRows(r.Username);
        set lss clientList;
        return <xml>
            <head>
                <title>WebRTC Channel</title>
                <link rel="stylesheet" type="text/css" href="/webrtc.css" />
            </head>
            <body onload={spawn (receiver())}>
                <dyn signal={v <- signal user; return <xml><h1 class={heading}>You are listening to {[v]} </h1></xml>}/>
                <h2>List of Active Clients</h2>
                <h4>Note : You won't see your channel. Please update client list when others get online.</h4>
                {dynTable lss}
                <br/>
                <button value="Update Client List"  onclick={fn _ => nl <- rpc(allRows(r.Username)); set lss nl}></button>
                <br/><br/><br/>
                <div><b>Messaging Snapshot</b></div>
                <br/>
                <dyn signal={vi <- signal msg; return <xml><div>{[vi]}</div></xml>}/>
               <dyn signal={Buffer.render buf}/>
            </body>
        </xml>
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
    <head>
        <title>WebRTC</title>
        <link rel="stylesheet" type="text/css" href="/webrtc.css" />
    </head>
    <body onload={spawn (eventHandler())}>
        <h1 class={heading}>Welcome to the WebRTC demo!</h1>
        <form>
            <textbox{#Username} placeholder="Enter a name for the channel" class={channelBox}/>
            <submit value="Create Channel" action={createChannel}/>
        </form>
        <br/>
        <button value="Click me please" onclick={fn _ => n <- JsWebrtcJs.myFunction "Nitin Surana Test"; n<- urWebFromDatastore "key"; alert n}></button>
    </body>
    </xml>
end