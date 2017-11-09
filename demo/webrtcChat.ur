table users : { Username: string }
  PRIMARY KEY Username

style channelBox
style heading
style activeClientsTable



fun allRows (uname) =
    query (SELECT users.Username FROM users WHERE users.Username <> {[uname]})
    (fn r acc => return (Cons ((r.Users.Username), acc)))
    Nil

fun channelBuffers (uname) =
    query (SELECT users.Username FROM users WHERE users.Username <> {[uname]})
    (fn r acc => 
            buff <-  Buffer.create; 
            msg <- source "";
            return (Cons ((r.Users.Username , buff , False ,  msg), acc)))
    Nil


fun createChannel r =
    user <- source r.Username;
    srcXML <- Webrtc.init r.Username;
    dml (INSERT INTO users (Username) VALUES ({[r.Username]}));
    lss <- source Nil;

    let

        fun updateConnectedClients (clientList, senderUsername, targetUsername, isConnectedFlag) =
            debug "Updating connected clients";
            updatedList <- List.mapM (fn (uname, buff, isConnected, msg) => 
                    debug uname;
                    if uname = targetUsername  then
                        return (uname, buff, isConnectedFlag, msg)
                    else
                        return (uname, buff, isConnected, msg)
                    ) clientList;
            debug "Updating connected clients"

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

        fun disconnect (sender, target) =
            debug "Hi";
            clientList <- get lss;
            updateConnectedClients(clientList, sender, target, True);
            writeToBuffer(clientList, target, "Hi");
            JsWebrtcChatJs.disconnect target

        fun handshake (sender, target) =
            Webrtc.connect(sender, target)

        fun sendWebRTCMessage (targetUsername, msg) =
            clientList <- get lss;
            writeToBuffer(clientList, targetUsername, "SEND :: " ^ msg);
            JsWebrtcChatJs.sendWebRTCMessage targetUsername msg

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
                <link rel="stylesheet" type="text/css" href="/webrtcChat.css" />
            </head>
            <body>
                <dyn signal={v <- signal user; return <xml><h1 class={heading}>You are {[v]} </h1></xml>}/>
                <h2>List of Active Clients</h2>
                <h4>Note : You won't see your channel. Please update client list when others get online.</h4>
                <br/>
                {dynTable lss}
                <br/>
                <button value="Update Client List"  onclick={fn _ => nl <- rpc(channelBuffers(r.Username)); set lss nl}></button>
                <br/><br/>
                <div><b>Messaging Snapshot</b></div>
                <br/>
                <span>{srcXML}</span>
            </body>
        </xml>
    end

fun main () =
    return <xml>
        <head>
            <title>WebRTC</title>
            <link rel="stylesheet" type="text/css" href="/webrtcChat.css" />
        </head>
        <body>
            <h1 class={heading}>Welcome to the WebRTC demo!</h1>
            <form>
                <textbox{#Username} placeholder="Enter a name for the channel" class={channelBox}/>
                <submit value="Create Channel" action={createChannel}/>
            </form>
        </body>
        </xml>