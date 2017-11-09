val init : string -> transaction xbody
val onMsgReceive : (string * string * string * string) -> transaction unit
val connect : (string * string * _ ) -> transaction unit
functor Make(M : sig
                 val onMsgReceiveCallback : _
             end) : sig
    val simple:string -> transaction string
end