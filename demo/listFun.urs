functor Make(M : sig
                 type t
             end) : sig
    val main : unit -> transaction page
end
