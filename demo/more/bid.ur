functor Make(M : Conference.INPUT) = struct
    open M

    table bid : {User : userId, Paper : paperId, Interest : char}
          PRIMARY KEY (User, Paper)

    table assignment : {User : userId, Paper : paperId}
          PRIMARY KEY (User, Paper)

    fun isOnPc id =
        ro <- oneOrNoRows1 (SELECT user.OnPc
                            FROM user
                            WHERE user.Id = {[id]});
        return (case ro of
                    None => False
                  | Some r => r.OnPc)

    val linksForPc =
        let
            fun bid () =
                me <- getLogin;
                return <xml>Bidding time!</xml>
        in
            <xml>
              <li> <a link={bid ()}>Bid on papers</a></li>
            </xml>
        end

    con yourPaperTables = [Assignment = _]
    constraint [Paper] ~ yourPaperTables
    fun joinYourPaper [tabs] [paper] [[Paper] ~ tabs] [[Paper] ~ _] [tabs ~ yourPaperTables] [[Id] ~ paper]
        (fi : sql_from_items ([Paper = [Id = paperId] ++ paper] ++ tabs)) =
        sql_inner_join fi (sql_from_table [#Assignment] assignment) (WHERE Paper.Id = Assignment.Paper)
end
