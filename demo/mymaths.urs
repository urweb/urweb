functor Make(M : sig
                 type data
                 val callback : _
             end) : sig

    val simple:string -> transaction string

end
