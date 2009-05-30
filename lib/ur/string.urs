type t = string

val length : t -> int

val append : t -> t -> t

val sub : t -> int -> char
val suffix : t -> int -> string

val index : t -> char -> option int
val atFirst : t -> char -> option string

val substring : t -> {Start : int, Len : int} -> string

val split : t -> char -> option (string * string)
