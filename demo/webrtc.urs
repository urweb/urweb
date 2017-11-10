functor Make(M : sig
                 val onMsgReceiveCallback : _
             end) : sig
    val init : string -> transaction xbody
	val onMsgReceive : (string * string * string * string) -> transaction unit
	val connect : (string * string) -> transaction unit
end
val makeChannel : string -> transaction unit
