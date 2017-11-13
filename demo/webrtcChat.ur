open Webrtc

table users : { Username: string }
  PRIMARY KEY Username

style channelBox
style heading
style btn
style btn_default
style btn_primary
style btn_info
style btn_success
style btn_warning
style form_control
style form_inline
style form_group
style container
style container_fluid
style pull_right
style pull_left
style table_striped
style table_responsive
style table_bordered
style btn_sm
style table_wrapper
style clearfix
style glyphicon
style glyphicon_refresh
style table_bootstrap
style col_xs_8
style col_xs_4
style w100


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
                                <button class="btn btn-sm btn-primary" value="WebRTC connect" data={data_attr data_kind "connect" uname} onclick={fn _ => handshake (r.Username , uname)}></button>
                                <button class="btn btn-sm btn-warning" value="WebRTC disconnect" data={data_attr data_kind "disconnect" uname} style="display: none;" onclick={fn _ => disconnect (r.Username , uname)}></button>
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
                            <div>
                                <ctextbox source={msg} placeholder="Enter message to chat" class="form-control"/>
                            </div>
                            <br/>
                            <div>
                                <button class="btn btn-sm btn-primary w100" value="Send" data={data_attr data_kind "message" uname} disabled onclick={fn _ => msgV <- get msg; sendWebRTCMessage(uname, msgV); set msg ""}></button>
                            </div>
                            </td>
                            {sendMsg ls}
                        </xml>
            in
                 <xml><dyn signal={ls <- signal xyz; return <xml>
                 <table border=1 class="table-bootstrap table-bordered table-striped table-responsive">
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
                <link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css" />
                <link rel="stylesheet" type="text/css" href="/webrtcChat.css" />
            </head>
            <body onload={initHandShake()}>
                <div class="container">
                <dyn signal={v <- signal user; return <xml><h1 class={heading}>You are {[v]} </h1></xml>}/>
                <div class="clearfix">
                        <h3>List of Active Clients</h3>
                <button class="btn btn-primary btn-sm pull-right" onclick={fn _ => nl <- rpc(channelBuffers(r.Username)); set lss nl}><i class="glyphicon glyphicon-refresh"/> Refresh List</button>
                        <h5>Note : You won't see your channel. Please refresh list to see new connected users.</h5>
                </div>
                <div class="clearfix"></div>
                <br/>
                <div class="table-wrapper">
                        {dynTable lss }
                    </div>
                <br/>
                <br/>
                <span><dyn signal={v <- signal srcXML; return <xml> {v}</xml>}/></span></div>
            </body>
        </xml>
    end

fun main () =
    return <xml>
        <head>
            <title>WebRTC</title>
            <link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css" />
            <link rel="stylesheet" type="text/css" href="/webrtcChat.css" />
        </head>
        <body>
            <div class="container">
                <h1 class={heading}>Welcome to the WebRTC demo!</h1>
                <form>
                    <textbox{#Username} placeholder="Please enter a name" class={channelBox}/>
                    <submit value="Create Channel" action={createChannel}/>
                </form>
            </div>
        </body>
        </xml>