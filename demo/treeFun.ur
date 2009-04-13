functor Make(M : sig
                 type key
                 con id :: Name
                 con parent :: Name
                 con cols :: {Type}
                 constraint [id] ~ [parent]
                 constraint [id, parent] ~ cols

                 val key_inj : sql_injectable_prim key

                 table tab : ([id = key, parent = option key] ++ cols)
             end) = struct

    open M

    fun tree (f : $([id = key, parent = option key] ++ cols) -> xbody)
             (root : option M.key) =
        let
            fun recurse (root : option key) =
                queryX' (SELECT * FROM tab WHERE {eqNullable' (SQL tab.{parent}) root})
                        (fn r =>
                            children <- recurse (Some r.Tab.id);
                            return <xml>
                              <li> {f r.Tab}</li>
                              
                              <ul>
                                {children}
                              </ul>
                            </xml>)
        in
            recurse root
        end

end
