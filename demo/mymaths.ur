functor Make(M : sig
                 val callback : _
             end) = struct
    val cb  = M.callback

    fun simple nn =
        cb(nn);
        return nn
end