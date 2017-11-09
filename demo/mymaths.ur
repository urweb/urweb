functor Make(M : sig
                 type data
                 val callback : string
             end) = struct

    val cb  = M.callback

    fun simple nn =
        return (nn ^ cb)
end