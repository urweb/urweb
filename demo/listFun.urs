functor Make(M : sig
                 type t
                 val toString : t -> string
                 val fromString : string -> option t
             end) : sig
    val main : unit -> transaction page
end
