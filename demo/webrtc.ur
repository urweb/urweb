table channels : { Username: string, Channel : channel (string * string * string * string) }
  PRIMARY KEY Username

fun sendPayload v =
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Username = {[v.3]});
    send r.Channels.Channel v


functor Make(M : sig
				 val onHandshakeCompleteCallback : _
                 val onMsgReceiveCallback : _
                 val onDisconnectCallback : _
             end) = struct
    val onMsgReceiveHandler = M.onMsgReceiveCallback
    val onDisconnectHandler = M.onDisconnectCallback
    val onHandshakeCompleteHandler = M.onHandshakeCompleteCallback

    fun eventHandler(senderUsername, targetUsername) =
	    sleep 200;
	    x <- JsWebrtcJs.getPendingEvent targetUsername;

	    if x = "undefined" then
	        eventHandler(senderUsername, targetUsername)
	    else if x = "offer-generated" then
	        y <- JsWebrtcJs.getDatastore targetUsername "offer";
	        debug "----In offer generated start---";
	        debug senderUsername;
	        debug targetUsername;
	        debug y;
	        rpc(sendPayload ("offer", senderUsername, targetUsername, y));
	        JsWebrtcJs.clearPendingEvent targetUsername x;
	        eventHandler(senderUsername, targetUsername)
	    else if x = "answer-generated" then
	    	debug "----In answer generated start---";
	        y <- JsWebrtcJs.getDatastore targetUsername "answer";
	        rpc(sendPayload ("answer", senderUsername, targetUsername, y));
	        JsWebrtcJs.clearPendingEvent targetUsername x;
	        eventHandler(senderUsername, targetUsername)
	    else if x = "ice-candidate-generated" then
	        y <- JsWebrtcJs.getDatastore targetUsername "ice-candidate";
	        rpc(sendPayload ("ice-candidate", senderUsername, targetUsername, y));
	        JsWebrtcJs.clearPendingEvent targetUsername x;            
	        eventHandler(senderUsername, targetUsername)
	    else if x = "message-received" then
	        y <- JsWebrtcJs.getDatastore targetUsername "message";
	        onMsgReceiveHandler targetUsername y;              
	        JsWebrtcJs.clearPendingEvent targetUsername x; 
	        eventHandler(senderUsername, targetUsername)
	    else if x = "handshake-complete" then
	        onHandshakeCompleteHandler senderUsername targetUsername;              
	        JsWebrtcJs.clearPendingEvent targetUsername x; 
	        eventHandler(senderUsername, targetUsername)
	    else if x = "disconnect" then
	        onDisconnectHandler senderUsername targetUsername;              
	        JsWebrtcJs.clearPendingEvent targetUsername x; 
	        eventHandler(senderUsername, targetUsername) 
	    else
	        eventHandler(senderUsername, targetUsername)

    fun onMsgReceive v =
    	debug "channel message-received";
    	debug v.1;
		if v.1 = "offer" then
			debug "----In create answer---";
	        spawn(eventHandler(v.3, v.2));
	        JsWebrtcJs.createAnswer v.2 v.4
	    else if v.1 = "answer" then
	    	debug "----In consume answer---";
	        JsWebrtcJs.consumeAnswer v.2 v.4
	    else if v.1 = "ice-candidate" then
	        JsWebrtcJs.consumeIceCandidate v.2 v.4
	    else
	        debug "unknown"

    fun receiver (ch) =
	    v <- recv ch;
	    onMsgReceive(v);
	    receiver(ch)

	fun getChannel username : transaction xbody =
		ch <- channel;
		dml (INSERT INTO channels (Username, Channel) VALUES ({[username]}, {[ch]}));
	    return <xml>
	    	<active code={spawn(receiver (ch));onDisconnect (alert "Server booted me"); return <xml/>}/>
	  </xml>

	fun init username =
    	x <- rpc(getChannel username);
    	return x

   	fun connect (senderUsername, targetUsername) =
   		debug "----In connect start---";
   		debug senderUsername;
   		debug targetUsername;
   		debug "----In connect end---";
	    spawn(eventHandler(senderUsername, targetUsername));
	    JsWebrtcJs.createOffer targetUsername

	fun disconnect (senderUsername, targetUsername) =
   		debug "----In disconnect start---";
   		debug senderUsername;
   		debug targetUsername;
   		debug "----In disconnect end---";
	    JsWebrtcJs.disconnect targetUsername

end
