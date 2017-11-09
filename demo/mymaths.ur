table channels : { Username: string, Channel : channel (string * string * string * string) }
  PRIMARY KEY Username

fun sendPayload v =
    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Username = {[v.3]});
    send r.Channels.Channel v

fun makeChannel username =
	ch <- channel;
    dml (INSERT INTO channels (Username, Channel) VALUES ({[username]}, {[ch]}))

functor Make(M : sig
                 val callback : _
             end) = struct
    val cb  = M.callback

    fun eventHandler(senderUsername, targetUsername) =
	    sleep 1000;
	    x <- JsWebrtcChatJs.getPendingEvent targetUsername;

	    if x = "undefined" then
	        eventHandler(senderUsername, targetUsername)
	    else if x = "offer-generated" then
	        y <- JsWebrtcChatJs.getDatastore targetUsername "offer";
	        debug "----In offer generated start---";
	        debug senderUsername;
	        debug targetUsername;
	        debug y;
	        rpc(sendPayload ("offer", senderUsername, targetUsername, y));
	        JsWebrtcChatJs.clearPendingEvent targetUsername x;
	        eventHandler(senderUsername, targetUsername)
	    else if x = "answer-generated" then
	    	debug "----In answer generated start---";
	        y <- JsWebrtcChatJs.getDatastore targetUsername "answer";
	        rpc(sendPayload ("answer", senderUsername, targetUsername, y));
	        JsWebrtcChatJs.clearPendingEvent targetUsername x;
	        eventHandler(senderUsername, targetUsername)
	    else if x = "ice-candidate-generated" then
	        y <- JsWebrtcChatJs.getDatastore targetUsername "ice-candidate";
	        rpc(sendPayload ("ice-candidate", senderUsername, targetUsername, y));
	        JsWebrtcChatJs.clearPendingEvent targetUsername x;            
	        eventHandler(senderUsername, targetUsername)
	    else if x = "message-received" then
	        y <- JsWebrtcChatJs.getDatastore targetUsername "message";
	        cb(targetUsername, y);              
	        JsWebrtcChatJs.clearPendingEvent targetUsername x; 
	        eventHandler(senderUsername, targetUsername) 
	    else
	        eventHandler(senderUsername, targetUsername)

    fun onMsgReceive v =
    	debug "channel message-received";
    	debug v.1;
		if v.1 = "offer" then
			debug "----In create answer---";
	        spawn(eventHandler(v.3, v.2));
	        JsWebrtcChatJs.createAnswer v.2 v.4
	    else if v.1 = "answer" then
	    	debug "----In consume answer---";
	        JsWebrtcChatJs.consumeAnswer v.2 v.4
	    else if v.1 = "ice-candidate" then
	        JsWebrtcChatJs.consumeIceCandidate v.2 v.4
	    else
	        debug "unknown"

    fun receiver (ch) =
	    v <- recv ch;
	    onMsgReceive(v);
	    receiver(ch)


	fun getChannel username : transaction xbody =
	    r <- oneRow (SELECT channels.Channel FROM channels WHERE channels.Username = {[username]});
	    return <xml>
	    	<active code={spawn(receiver (r.Channels.Channel)); return <xml/>}/>
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
	    JsWebrtcChatJs.createOffer targetUsername

end
