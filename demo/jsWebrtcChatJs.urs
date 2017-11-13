val onMsgReceive : string -> string -> transaction unit
val onMsgSend : string -> string -> transaction unit
val onDisconnect : string -> string -> transaction unit
val onHandshakeComplete : string -> string -> transaction unit