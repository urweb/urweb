functor Make(M : Conference.INPUT) = struct
    open M

    table bid : {User : userId, Paper : paperId, Interest : char}
          PRIMARY KEY (User, Paper)

    table assignment : {User : userId, Paper : paperId}
          PRIMARY KEY (User, Paper)

    val linksForPc =
        let
            fun yourBids () =
                me <- getPcLogin;
                ps <- queryX (SELECT paper.Id, paper.{{M.paper}}, bid.Interest
                              FROM paper LEFT JOIN bid ON bid.Paper = paper.Id
                                AND bid.User = {[me.Id]})
                             (fn r => <xml><entry>
                               <hidden{#Paper} value={show r.Paper.Id}/>
                               {useMore <xml><tr>
                                 <td>{summarizePaper (r.Paper -- #Id)}</td>
                                 <td><select{#Bid}>
                                   {Select.selectChar ((#"-", "No") :: (#"_", "Maybe") :: (#"+", "Yes") :: [])
                                                      r.Bid.Interest}
                                 </select></td>
                               </tr></xml>}
                             </entry></xml>);
                return <xml><body>
                  <h1>Bid on papers</h1>

                  <form>
                    <subforms{#Papers}><table>
                      <tr> <th>Paper</th> <th>Your Bid</th> </tr>
                      {ps}
                    </table></subforms>
                    <submit value="Change" action={changeBids}/>
                  </form>
                </body></xml>

            and changeBids r =
                me <- getPcLogin;
                List.app (fn {Paper = p, Bid = b} =>
                             case b of
                                 "" => return ()
                               | _ => let
                                     val p = readError p
                                 in
                                     (dml (DELETE FROM bid WHERE Paper = {[p]} AND User = {[me.Id]});
                                      dml (INSERT INTO bid (Paper, User, Interest)
                                           VALUES ({[p]}, {[me.Id]}, {[String.sub b 0]})))
                                 end) r.Papers;
                yourBids ()
        in
            <xml>
              <li> <a link={yourBids ()}>Bid on papers</a></li>
            </xml>
        end

    con yourPaperTables = [Assignment = _]
    constraint [Paper] ~ yourPaperTables
    fun joinYourPaper [tabs] [paper] [[Paper] ~ tabs] [[Paper] ~ _] [tabs ~ yourPaperTables] [[Id] ~ paper]
        (fi : sql_from_items ([Paper = [Id = paperId] ++ paper] ++ tabs)) =
        sql_inner_join fi (sql_from_table [#Assignment] assignment) (WHERE Paper.Id = Assignment.Paper)
end
