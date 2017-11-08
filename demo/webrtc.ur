table channels : { Client : client, Username: string, Channel : channel (string * string * string * string) }
  PRIMARY KEY Client

style channelBox
style heading
style activeClientsTable



fun allRows (uname) =
    query (SELECT channels.Username FROM channels WHERE channels.Username <> {[uname]})
    (fn r acc => return (Cons ((r.Channels.Username), acc)))
    Nil

fun channelBuffers (uname) =
    query (SELECT channels.Username FROM channels WHERE channels.Username <> {[uname]})
    (fn r acc => 
            buff <-  Buffer.create; 
            msg <- source "";
            return (Cons ((r.Channels.Username , buff , False ,  msg), acc)))
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

    let

        fun updateConnectedClients (clientList, senderUsername, targetUsername, isConnectedFlag) =
            debug "Updating connected clients";
            updatedList <- List.mapM (fn (uname, buff, isConnected, msg) => 
                    debug uname;
                    if uname = targetUsername  then
                        return (uname, buff, isConnectedFlag, msg)
                    else if uname = senderUsername  then
                        return (uname, buff, isConnectedFlag, msg)
                    else
                        return (uname, buff, isConnected, msg)
                    ) clientList;
            set lss updatedList

        fun writeToBuffer (clientList, targetUsername, y) =
                    case clientList of
                     Nil => debug targetUsername
                    | Cons ((uname, buff, isConnected, msg), ls) =>
                        if uname = targetUsername then
                            debug "Here";
                            Buffer.write buff (y)
                        else
                            debug "Not Here";
                            debug targetUsername;
                            writeToBuffer(ls, targetUsername, y)

        fun eventHandler(targetUsername) =
            sleep 1000;
            x <- JsWebrtcJs.getPendingEvent targetUsername;
            senderUsername <- get user;

            if x = "undefined" then
                eventHandler(targetUsername)
            else if x = "offer-generated" then
                y <- JsWebrtcJs.getDatastore targetUsername "offer";
                rpc(sendPayload ("offer", senderUsername, targetUsername, y));
                JsWebrtcJs.clearPendingEvent targetUsername x;
                eventHandler(targetUsername)
            else if x = "answer-generated" then
                y <- JsWebrtcJs.getDatastore targetUsername "answer";
                rpc(sendPayload ("answer", senderUsername, targetUsername, y));
                JsWebrtcJs.clearPendingEvent targetUsername x;
                eventHandler(targetUsername)
            else if x = "ice-candidate-generated" then
                y <- JsWebrtcJs.getDatastore targetUsername "ice-candidate";
                rpc(sendPayload ("ice-candidate", senderUsername, targetUsername, y));
                JsWebrtcJs.clearPendingEvent targetUsername x;            
                eventHandler(targetUsername)
            else if x = "handshake-complete" then
                debug "handshake is complete";
                clientList <- get lss;
                newList <- updateConnectedClients(clientList, senderUsername, targetUsername, True);
                JsWebrtcJs.clearPendingEvent targetUsername x;
                eventHandler(targetUsername)
            else if x = "disconnect" then
                debug "client disconnected";
                clientList <- get lss;
                newList <- updateConnectedClients(clientList, senderUsername, targetUsername, False);
                JsWebrtcJs.clearPendingEvent targetUsername x;
                eventHandler(targetUsername)
            else if x = "message-received" then
                y <- JsWebrtcJs.getDatastore targetUsername "message";
                clientList <- get lss;
                Buffer.write buf (y);          
                writeToBuffer(clientList, targetUsername, "RECEIVE :: " ^ y);              
                JsWebrtcJs.clearPendingEvent targetUsername x; 
                eventHandler(targetUsername) 
            else
                eventHandler(targetUsername)


        fun handshake (sender, target) =
            spawn(eventHandler(target));
            JsWebrtcJs.createOffer target

        fun disconnect (sender, target) =
            JsWebrtcJs.disconnect target

        fun sendWebRTCMessage (targetUsername, msg) =
            clientList <- get lss;
            writeToBuffer(clientList, targetUsername, "SEND :: " ^ msg);
            JsWebrtcJs.sendWebRTCMessage targetUsername msg

        fun onMsgReceive v =
            if v.1 = "offer" then
                spawn(eventHandler(v.2));
                JsWebrtcJs.createAnswer v.2 v.4
            else if v.1 = "answer" then
                JsWebrtcJs.consumeAnswer v.2 v.4
            else if v.1 = "ice-candidate" then
                JsWebrtcJs.consumeIceCandidate v.2 v.4
            else
                Buffer.write buf ("unknown")

        fun receiver () =
            v <- recv ch;
            username <- get user;
            set msg "";
            onMsgReceive(v);
            receiver()


        fun dynTable xyz =
            let
                fun dispBtn (isConnected, senderUsername, targetUsername) =
                    case isConnected of
                        True => <xml><button value="WebRTC disconnect" onclick={fn _ => disconnect (senderUsername , targetUsername)}></button></xml>
                        | False => <xml><button value="WebRTC connect" onclick={fn _ => handshake (senderUsername , targetUsername)}></button></xml>

                fun dispMsgBtn (isConnected, uname, msg) =
                    case isConnected of
                        True => <xml><button value="WebRTC message" onclick={fn _ => msgV <- get msg; sendWebRTCMessage(uname, msgV); set msg "" }></button></xml>
                        | False => <xml><button value="WebRTC message" disabled onclick={fn _ => msgV <- get msg; sendWebRTCMessage(uname, msgV)}></button></xml>

                fun dispAction v =
                    case v of
                     Nil => <xml/>
                    | Cons ((uname, buff, isConnected, msg), ls) =>
                        <xml>
                            <td>
                                {dispBtn(isConnected,r.Username,uname)}
                            </td>
                            {dispAction ls}
                        </xml>

                fun dispName v =
                    case v of
                     Nil => <xml/>
                    | Cons ((uname, buff, isConnected, msg), ls) =>
                        <xml>
                            <td>{[uname]}</td>
                            {dispName ls}
                        </xml>

                fun dispMsg v =
                    case v of
                     Nil => <xml/>
                    | Cons ((uname, buff, isConnected, msg), ls) =>
                        <xml>
                            <td>
                                <div><dyn signal={Buffer.render buff}/></div>
                            </td>
                            {dispMsg ls}
                        </xml>

                fun sendMsg v =
                    case v of
                     Nil => <xml/>
                    | Cons ((uname, buff, isConnected, msg), ls) =>
                        <xml>
                            <td>
                            <ctextbox source={msg} placeholder="Enter message to chat" />
                            {dispMsgBtn(isConnected, uname, msg)}
                            </td>
                            {sendMsg ls}
                        </xml>
            in
                 <xml><dyn signal={ls <- signal xyz; return <xml>
                 <table border=1 class={activeClientsTable}>
                    <tr>
                           {dispName ls}
                    </tr>
                    <tr>
                           {dispAction ls}
                    </tr>
                    <tr>
                           {dispMsg ls}
                    </tr>
                    <tr>
                           {sendMsg ls}
                    </tr>
                     
                </table>
                </xml>}/></xml>
            end

    in
        clientList <- channelBuffers(r.Username);
        set lss clientList;
        return <xml>
            <head>
                <title>WebRTC Channel</title>
                <link rel="stylesheet" type="text/css" href="/webrtc.css" />
            </head>
            <body onload={spawn (receiver())}>
                <dyn signal={v <- signal user; return <xml><h1 class={heading}>You are {[v]} </h1></xml>}/>
                <h2>List of Active Clients</h2>
                <h4>Note : You won't see your channel. Please update client list when others get online.</h4>
                {dynTable lss}
                <br/>
                <button value="Update Client List"  onclick={fn _ => nl <- rpc(channelBuffers(r.Username)); set lss nl}></button>
                <br/><br/>
                <br/>
                <div><b>Messaging Snapshot</b></div>
                <br/>
                <dyn signal={vi <- signal msg; return <xml><div>{[vi]}</div></xml>}/>
            </body>
        </xml>
    end


fun main () =
    return <xml>
        <head>
            <title>WebRTC</title>
            <link rel="stylesheet" type="text/css" href="/webrtc.css" />
        </head>
        <body>
            <h1 class={heading}>Welcome to the WebRTC demo!</h1>
            <form>
                <textbox{#Username} placeholder="Enter a name for the channel" class={channelBox}/>
                <submit value="Create Channel" action={createChannel}/>
            </form>
        </body>
        </xml>