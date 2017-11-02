datatype list t = Nil | Cons of t * list t

table channels : { Client : client, Username: string, Channel : channel (string * string * string * string) }
  PRIMARY KEY Client

style channelBox
style heading
style activeClientsTable



fun allRows (uname) =
    query (SELECT channels.Username FROM channels WHERE channels.Username <> {[uname]})
    (fn r acc => return (Cons ((r.Channels.Username), acc)))
    Nil


fun sendPayload v =
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Username = {[v.3]});
    send r.Channels.Channel v


fun createChannel r =
    me <- self;
    ch <- channel;
    dml (INSERT INTO channels (Client, Username, Channel) VALUES ({[me]}, {[readError r.Username]}, {[ch]}));
    buf <- Buffer.create;
    src <- source 1;
    user <- source r.Username;
    lss <- source Nil;
    msg <- source "No messages so far!";
    targetUser <- source "";

    let

        fun eventHandler() =
            sleep 1000;
            x <- JsWebrtcJs.getPendingEvent();
            senderUsername <- get user;
            targetUsername <- get targetUser;
            if x = "undefined" then
                eventHandler()
            else if x = "offer-generated" then
                y <- JsWebrtcJs.getDatastore "offer";
                debug "Offer";
                debug y;
                JsWebrtcJs.clearPendingEvent();
                debug "Sender";
                debug senderUsername;
                debug "Target";
                debug targetUsername;
                rpc(sendPayload ("offer", senderUsername, targetUsername, y));
                eventHandler()
            else if x = "answer-generated" then
                y <- JsWebrtcJs.getDatastore "answer";
                debug "Answer";
                debug y;
                JsWebrtcJs.clearPendingEvent();
                debug "Sender";
                debug senderUsername;
                debug "Target";
                debug targetUsername;
                rpc(sendPayload ("answer", senderUsername, targetUsername, y));
                eventHandler()
            else
                debug x;
                debug "Sender";
                debug senderUsername;
                debug "Target";
                debug targetUsername;
                JsWebrtcJs.clearPendingEvent();
                eventHandler()


        fun handshake (sender, target) =
            set targetUser target;
            JsWebrtcJs.createOffer sender

        fun onMsgReceive v =
            targetUsername <- get targetUser;
            set targetUser v.2;
            if v.1 = "offer" then
                Buffer.write buf ("offer");
                JsWebrtcJs.createAnswer (v.3 ^ ":::" ^ v.4)
            else if v.1 = "answer" then
                Buffer.write buf ("answer");
                JsWebrtcJs.consumeAnswer (v.3 ^ ":::" ^ v.4)
            else if v.1 = "ice-candidate" then
                iceCandidate <- JsWebrtcJs.getDatastore "ice-candidate";
                Buffer.write buf ("ice-candidate");
                JsWebrtcJs.consumeIceCandidate (v.3 ^ ":::" ^ iceCandidate)
            else
                Buffer.write buf ("unknown")

        fun receiver () =
            v <- recv ch;
            username <- get user;
            set msg "";
            onMsgReceive(v);
            Buffer.write buf ("(Received " ^ v.1 ^ " from " ^ v.2 ^" : " ^ show v.4 ^ ")");
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
                                    <button value="WebRTC connect" onclick={fn _ => handshake (r.Username,uname)}></button>
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
            <body onload={spawn (receiver()); spawn (eventHandler())}>
                <dyn signal={v <- signal user; return <xml><h1 class={heading}>You are listening to {[v]} </h1></xml>}/>
                <h2>List of Active Clients</h2>
                <h4>Note : You won't see your channel. Please update client list when others get online.</h4>
                {dynTable lss}
                <br/>
                <button value="Update Client List"  onclick={fn _ => nl <- rpc(allRows(r.Username)); set lss nl}></button>
                <br/><br/>
                <br/>
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
        else if x = "offer-generated" then
            y <- JsWebrtcJs.getDatastore "offer";
            debug "Offer";
            debug y;
            JsWebrtcJs.clearPendingEvent();
            rpc(sendPayload ("offer", "123", "123", y));
            eventHandler()
        else if x = "answer-generated" then
            y <- JsWebrtcJs.getDatastore "answer";
            debug "Answer";
            debug y;
            JsWebrtcJs.clearPendingEvent();
            eventHandler()
        else
            debug x;
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
        <button value="Create Offer" onclick={fn _ => JsWebrtcJs.createOffer "123"}></button>
    </body>
    </xml>
end