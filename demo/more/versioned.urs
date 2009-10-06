functor Make(M : sig
                 con key :: {Type}
                 con data :: {Type}
                 constraint key ~ data
                 constraint [When, Version] ~ (key ++ data)

                 val key : $(map sql_injectable key)
                 val data : $(map (fn t => {Inj : sql_injectable_prim t,
                                            Eq : eq t}) data)

                 val keyFolder : folder key
                 val dataFolder : folder data
             end) : sig
    val insert : $(M.key ++ M.data) -> transaction unit
    val update : $(M.key ++ M.data) -> transaction unit

    val keys : transaction (list $M.key)
    val current : $M.key -> transaction (option $M.data)

    type version
    val keysAt : version -> transaction (list $M.key)
    val archive : version -> $M.key -> transaction (option $M.data)
    val updateTimes : transaction (list (version * time))
end
