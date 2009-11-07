signature INPUT = sig
    con paper :: {Type}
    constraint [Id, Document] ~ paper

    type userId
    val userId_inj : sql_injectable_prim userId
    table user : {Id : userId, Nam : string, Password : string, Chair : bool, OnPc : bool}
                     PRIMARY KEY Id,
          CONSTRAINT Nam UNIQUE Nam

    type paperId
    val paperId_inj : sql_injectable_prim paperId
    val paperId_show : show paperId
    val paperId_read : read paperId
    val paperId_eq : eq paperId
    table paper : ([Id = paperId, Document = blob] ++ paper)
                      PRIMARY KEY Id

    con review :: {Type}
    constraint [Paper, User] ~ review
    table review : ([Paper = paperId, User = userId] ++ review)
                       PRIMARY KEY (Paper, User)

    val checkLogin : transaction (option {Id : userId, Nam : string, Chair : bool, OnPc : bool})
    val getLogin : transaction {Id : userId, Nam : string, Chair : bool, OnPc : bool}
    val getPcLogin : transaction {Id : userId, Nam : string, Chair : bool}
    val checkChair : transaction unit
    val summarizePaper : ctx ::: {Unit} -> [[Body] ~ ctx] => $paper -> xml ([Body] ++ ctx) [] []
end

signature OUTPUT = sig
    con paper :: {Type}
    type userId
    type paperId

    val linksForPc : xbody
    val linksForChair : xbody

    con yourPaperTables :: {{Type}}
    constraint [Paper] ~ yourPaperTables
    val joinYourPaper : tabs ::: {{Type}} -> paper ::: {Type}
        -> [[Paper] ~ tabs] => [[Paper] ~ yourPaperTables] => [tabs ~ yourPaperTables] => [[Id] ~ paper] =>
        userId (* Current user *)
        -> sql_from_items ([Paper = [Id = paperId] ++ paper] ++ tabs)
        -> sql_from_items (yourPaperTables ++ [Paper = [Id = paperId] ++ paper] ++ tabs)
end

functor Make(M : sig
                 con paper :: {(Type * Type)}
                 constraint [Id, Document, Authors] ~ paper
                 val paper : $(map Meta.meta paper)
                 val paperFolder : folder paper

                 con paperPrivate :: {Type}
                 constraint [Id, Document, Authors] ~ paperPrivate
                 constraint paper ~ paperPrivate
                 val paperPrivate : $(map Meta.private paperPrivate)
                 val paperPrivateFolder : folder paperPrivate

                 con review :: {(Type * Type)}
                 constraint [Paper, User] ~ review
                 val review : $(map Meta.meta review)
                 val reviewFolder : folder review

                 val submissionDeadline : time
                 val summarizePaper : ctx ::: {Unit} -> [[Body] ~ ctx] => $(map fst paper ++ paperPrivate)
                                                                          -> xml ([Body] ++ ctx) [] []

                 functor Make (M : INPUT where con paper = map fst paper ++ paperPrivate
                                         where con review = map fst review)
                         : OUTPUT where con paper = map fst paper ++ paperPrivate
                                  where con userId = M.userId
                                  where con paperId = M.paperId
             end) : sig

    val main : unit -> transaction page

end

functor Join(M : sig
                 structure O1 : OUTPUT

                 structure O2 : OUTPUT where con paper = O1.paper
                                       where con userId = O1.userId
                                       where con paperId = O1.paperId

                 constraint O1.yourPaperTables ~ O2.yourPaperTables
             end) : OUTPUT where con paper = M.O1.paper
                           where con userId = M.O1.userId
                           where con paperId = M.O1.paperId
                           where con yourPaperTables = M.O1.yourPaperTables ++ M.O2.yourPaperTables
