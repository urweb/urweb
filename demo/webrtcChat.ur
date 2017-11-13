open Webrtc

table users : { Username: string }
  PRIMARY KEY Username

style channelBox
style heading
style activeClientsTable


fun channelBuffers (uname) =
    query (SELECT users.Username FROM users WHERE users.Username <> {[uname]})
    (fn r acc => 
            buff <-  Buffer.create; 
            msg <- source "";
            return (Cons ((r.Users.Username , buff , msg), acc)))
    Nil


structure AB = Webrtc.Make(struct
                               val onHandshakeCompleteCallback = JsWebrtcChatJs.onHandshakeComplete
                               val onMsgReceiveCallback = JsWebrtcChatJs.onMsgReceive
                               val onDisconnectCallback = JsWebrtcChatJs.onDisconnect
                           end)

fun createChannel r =
    user <- source r.Username;
    Webrtc.makeChannel(r.Username);
    srcXML <- source <xml/>;
    dml (INSERT INTO users (Username) VALUES ({[r.Username]}));
    lss <- source Nil;

    let

        fun initHandShake () =
            retXML <- AB.init r.Username;
            set srcXML retXML

        fun handshake (sender, target) =
            AB.connect(sender, target)

        fun disconnect (sender, target) =
            AB.disconnect(sender, target)

        fun sendWebRTCMessage (targetUsername, msg) =
            clientList <- get lss;
            JsWebrtcJs.sendWebRTCMessage targetUsername msg;
            JsWebrtcChatJs.onMsgSend targetUsername msg

        fun dynTable xyz =
            let
                fun dispAction v =
                    case v of
                     Nil => <xml/>
                    | Cons ((uname, buff, msg), ls) =>
                        <xml>
                            <td>
                                <button value="WebRTC connect" data={data_attr data_kind "connect" uname} onclick={fn _ => handshake (r.Username , uname)}></button>
                                <button value="WebRTC disconnect" data={data_attr data_kind "disconnect" uname} style="display: none;" onclick={fn _ => disconnect (r.Username , uname)}></button>
                            </td>
                            {dispAction ls}
                        </xml>

                fun dispName v =
                    case v of
                     Nil => <xml/>
                    | Cons ((uname, buff, msg), ls) =>
                        <xml>
                            <td>{[uname]}</td>
                            {dispName ls}
                        </xml>

                fun dispMsg v =
                    case v of
                     Nil => <xml/>
                    | Cons ((uname, buff, msg), ls) =>
                        <xml>
                            <td>
                                <div data={data_attr data_kind "id" uname}><dyn signal={Buffer.render buff}/></div>
                            </td>
                            {dispMsg ls}
                        </xml>

                fun sendMsg v =
                    case v of
                     Nil => <xml/>
                    | Cons ((uname, buff, msg), ls) =>
                        <xml>
                            <td>
                            <ctextbox source={msg} placeholder="Enter message to chat" />
                            <button value="WebRTC message" data={data_attr data_kind "message" uname} disabled onclick={fn _ => msgV <- get msg; sendWebRTCMessage(uname, msgV); set msg ""}></button>
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
            <body onload={initHandShake()}>
                
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
                <span><dyn signal={v <- signal srcXML; return <xml> {v}</xml>}/></span>
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