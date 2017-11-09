table channels : { Username: string, Channel : channel (string * string * string * string) }
  PRIMARY KEY Username


fun sendPayload v =
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Username = {[v.3]});
    send r.Channels.Channel v

fun eventHandler(senderUsername, targetUsername) =
    sleep 1000;
    x <- JsWebrtcChatJs.getPendingEvent targetUsername;

    if x = "undefined" then
        eventHandler(senderUsername, targetUsername)
    else if x = "offer-generated" then
        y <- JsWebrtcChatJs.getDatastore targetUsername "offer";
        rpc(sendPayload ("offer", senderUsername, targetUsername, y));
        JsWebrtcChatJs.clearPendingEvent targetUsername x;
        eventHandler(senderUsername, targetUsername)
    else if x = "answer-generated" then
        y <- JsWebrtcChatJs.getDatastore targetUsername "answer";
        rpc(sendPayload ("answer", senderUsername, targetUsername, y));
        JsWebrtcChatJs.clearPendingEvent targetUsername x;
        eventHandler(senderUsername, targetUsername)
    else if x = "ice-candidate-generated" then
        y <- JsWebrtcChatJs.getDatastore targetUsername "ice-candidate";
        rpc(sendPayload ("ice-candidate", senderUsername, targetUsername, y));
        JsWebrtcChatJs.clearPendingEvent targetUsername x;            
        eventHandler(senderUsername, targetUsername)
    (* 
    else if x = "handshake-complete" then
        debug "handshake is complete";
        clientList <- get lss;
        newList <- updateConnectedClients(clientList, senderUsername, targetUsername, True);
        JsWebrtcChatJs.clearPendingEvent targetUsername x;
        eventHandler(senderUsername, targetUsername)
    else if x = "disconnect" then
        debug "client disconnected";
        clientList <- get lss;
        newList <- updateConnectedClients(clientList, senderUsername, targetUsername, False);
        JsWebrtcChatJs.clearPendingEvent targetUsername x;
        eventHandler(senderUsername, targetUsername)
    else if x = "message-received" then
        y <- JsWebrtcChatJs.getDatastore targetUsername "message";
        clientList <- get lss;
        Buffer.write buf (y);          
        writeToBuffer(clientList, targetUsername, "RECEIVE :: " ^ y);              
        JsWebrtcChatJs.clearPendingEvent targetUsername x; 
        eventHandler(senderUsername, targetUsername)  
    *)
    else
        eventHandler(senderUsername, targetUsername)

fun onMsgReceive v =
	if v.1 = "offer" then
        spawn(eventHandler(v.3, v.2));
        JsWebrtcChatJs.createAnswer v.2 v.4
    else if v.1 = "answer" then
        JsWebrtcChatJs.consumeAnswer v.2 v.4
    else if v.1 = "ice-candidate" then
        JsWebrtcChatJs.consumeIceCandidate v.2 v.4
    else
        debug "unknown"

fun connect (senderUsername, targetUsername) =
    spawn(eventHandler(senderUsername, targetUsername));
    JsWebrtcChatJs.createOffer targetUsername

fun receiver (ch) =
    v <- recv ch;
    onMsgReceive(v);
    receiver(ch)

fun makeChannel username : transaction xbody =
	ch <- channel;
    dml (INSERT INTO channels (Username, Channel) VALUES ({[username]}, {[ch]}));
    return <xml>
    	<active code={spawn(receiver ch); return <xml/>}/>
  </xml>
    
fun init username =
	x <- makeChannel(username);
	return x

