functor Make(M : sig
                 type data
                 val inj : sql_injectable data
             end) : sig
    type ref
    val new : M.data -> transaction ref
    val read : ref -> transaction M.data
    val write : ref -> M.data -> transaction unit
    val delete : ref -> transaction unit
end
