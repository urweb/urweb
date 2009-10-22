functor Make(M : sig
                 con paper :: {(Type * Type)}
                 constraint [Id] ~ paper
                 val paper : $(map Meta.meta paper)
                 val paperFolder : folder paper

                 con review :: {(Type * Type)}
                 constraint [Paper, User] ~ review
                 val review : $(map Meta.meta review)

                 val submissionDeadline : time
             end) : sig

    val main : unit -> transaction page

end
