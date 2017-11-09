val sendWebRTCMessage : string -> string -> transaction unit
val getDatastore : string -> string -> transaction string
val getPendingEvent : string -> transaction string
val clearPendingEvent : string -> string -> transaction unit
val createOffer : string -> transaction unit
val createAnswer : string -> string -> transaction unit
val consumeAnswer : string -> string -> transaction unit
val consumeIceCandidate : string -> string -> transaction unit
val disconnect : string -> transaction unit
val debugMe : string * string -> transaction unit