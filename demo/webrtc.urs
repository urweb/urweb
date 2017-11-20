functor Make(M : sig
				 val onHandshakeCompleteCallback : _
                 val onMsgReceiveCallback : _
                 val onDisconnectCallback : _
             end) : sig
    val init : string -> transaction xbody
	val onMsgReceive : (string * string * string * string) -> transaction unit
	val connect : (string * string) -> transaction unit
	val disconnect : (string * string) -> transaction unit
end
