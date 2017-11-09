functor Make(M : sig
                 type data
                 val callback:string
             end) : sig

    val simple:string -> transaction string

end
