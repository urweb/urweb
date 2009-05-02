type t

val create : string -> t
val out : t -> string
val frob : t -> string -> t
val print : transaction unit

val foo : transaction unit
val bar : string -> transaction unit

val transactional : transaction unit
