functor Make(M : sig
                 con keyName :: Name
                 con keyType :: Type
                 val showKey : show keyType

                 con visible :: {(Type * Type)}
                 constraint [keyName] ~ visible
                 val folder : folder visible
                 val visible : $(map Meta.meta visible)

                 con invisible :: {Type}
                 constraint [keyName] ~ invisible
                 constraint visible ~ invisible

                 val title : string
                 val isAllowed : transaction bool
                 table t : ([keyName = keyType] ++ map fst visible ++ invisible)
             end) : sig

    val main : unit -> transaction page

end
