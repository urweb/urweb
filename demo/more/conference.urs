functor Make(M : sig
                 con paper :: {(Type * Type)}
                 constraint [Id, Document] ~ paper
                 val paper : $(map Meta.meta paper)
                 val paperFolder : folder paper

                 con review :: {(Type * Type)}
                 constraint [Paper, User] ~ review
                 val review : $(map Meta.meta review)

                 val submissionDeadline : time
                 val summarizePaper : $(map fst paper) -> xbody
             end) : sig

    val main : unit -> transaction page

end
