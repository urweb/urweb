type t

val create : transaction t
val render : t -> signal xbody
val write : t -> string -> transaction unit
