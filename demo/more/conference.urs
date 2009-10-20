functor Make(M : sig
                 con paper :: {(Type * Type)}
                 constraint [Id, Title] ~ paper
                 val paper : $(map Meta.meta paper)

                 con review :: {(Type * Type)}
                 constraint [Paper, User] ~ review
                 val review : $(map Meta.meta review)
             end) : sig

    val main : unit -> transaction page

end
