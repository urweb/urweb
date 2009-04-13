functor Make(M : sig
                 type key
                 con id :: Name
                 con parent :: Name
                 con cols :: {Type}
                 constraint [id] ~ [parent]
                 constraint [id, parent] ~ cols

                 val key_inj : sql_injectable_prim key

                 table tab : ([id = key, parent = option key] ++ cols)
             end) : sig

    con id = M.id
    con parent = M.parent

    val tree : ($([id = M.key, parent = option M.key] ++ M.cols) -> xbody)
               -> option M.key
               -> transaction xbody

end
