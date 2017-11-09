functor Make(M : sig
                 type data
                 val callback : _
             end) = struct

    val cb  = M.callback

    fun simple nn =
        cb(nn);
        return nn
end