val myFunction : string -> transaction string
val getDatastore : string -> transaction string
val getPendingEvent : unit -> transaction string
val clearPendingEvent :unit -> transaction unit
val createOffer : string -> transaction unit
val createAnswer : string -> transaction unit
val consumeAnswer : string -> transaction unit
val consumeIceCandidate : string -> transaction unit
