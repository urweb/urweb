functor Make(M : sig
                 val callback : _
             end) : sig
    val simple:string -> transaction string
end
