con fields userId paperId = [User = userId, Paper = paperId]

functor Make(M : Conference.INPUT) = struct
    open M

    table bid : {User : userId, Paper : paperId, Interest : char}
          PRIMARY KEY (User, Paper)

    table assignment : {User : userId, Paper : paperId}
          PRIMARY KEY (User, Paper)

    fun intOut ch =
        case ch of
            #"_" => "Maybe"
          | #"-" => "No"
          | #"+" => "Yes"
          | _ => error <xml>Bid: Invalid Interest code</xml>

    val linksForChair =
        let
            fun assignPapers () =
                tup <- query (SELECT paper.Id, paper.{{M.paper}}, user.Id, user.Nam, bid.Interest, assignment.User
                              FROM paper JOIN bid ON bid.Paper = paper.Id
                                JOIN user ON bid.User = user.Id
                                LEFT JOIN assignment ON assignment.Paper = paper.Id AND assignment.User = user.Id
                              ORDER BY paper.Id, bid.Interest, user.Nam)
                       (fn r (paper, int, acc, ints, papers) =>
                           if (case paper of None => False | Some r' => r'.Id = r.Paper.Id) then
                               if int = r.Bid.Interest then
                                   return (paper, int, (r.User.Id, r.User.Nam, Option.isSome r.Assignment.User) :: acc,
                                           ints, papers)
                               else
                                   return (paper, r.Bid.Interest, (r.User.Id, r.User.Nam,
                                                                   Option.isSome r.Assignment.User) :: [],
                                           (int, acc) :: ints, papers)
                           else
                               return (Some r.Paper, r.Bid.Interest,
                                       (r.User.Id, r.User.Nam, Option.isSome r.Assignment.User) :: [], [],
                                       case paper of
                                           None => papers
                                         | Some r => (r.Id, r -- #Id, (int, acc) :: ints) :: papers))
                       (None, #" ", [], [], []);
                let
                    val papersL = case tup.1 of
                                      Some r => (r.Id, r -- #Id, (tup.2, tup.3) :: tup.4) :: tup.5
                                    | None => []

                    fun makePapers () = List.mapM (fn (pid, extra, ints) =>
                                                      ints <- List.mapM (fn (int, users) =>
                                                                            cg <- CheckGroup.create
                                                                                      (List.mp
                                                                                           (fn (id, nam, sel) =>
                                                                                               (id, txt nam, sel))
                                                                                           users);
                                                                            ex <- Expandable.create
                                                                                      (CheckGroup.render cg);
                                                                            return (int, cg, ex)) ints;
                                                      return (pid, extra, ints)) papersL

                    fun saveAssignment ls =
                        dml (DELETE FROM assignment WHERE TRUE);
                        List.app (fn (pid, uids) =>
                                     List.app (fn uid => dml (INSERT INTO assignment (Paper, User)
                                                              VALUES ({[pid]}, {[uid]}))) uids) ls
                in
                    papers <- source [];

                    return <xml><body onload={papersL <- makePapers ();
                                              set papers papersL}>
                      <h1>Assign papers</h1>

                      <dyn signal={papers <- signal papers;
                                   return (List.mapX (fn (pid, extra, ints) => <xml>
                                     <hr/>
                                     #{[pid]}: {summarizePaper extra}:
                                     <dyn signal={n <- List.foldl (fn (_, cg, _) total =>
                                                                      this <- CheckGroup.selected cg;
                                                                      total <- total;
                                                                      return (List.length this + total)) (return 0) ints;
                                                  return (txt n)}/><br/>
                                       
                                     {List.mapX (fn (int, _, ex) => <xml>
                                       {[intOut int]}: {Expandable.render ex}
                                     </xml>) ints}
                                   </xml>) papers)}/>

                        <br/>
                        <button value="Save" onclick={papers <- get papers;
                                                      ls <- List.mapM (fn (pid, _, ints) =>
                                                                          ints <- List.mapM (fn (_, cg, _) =>
                                                                                                current
                                                                                                (CheckGroup.selected cg))
                                                                                            ints;
                                                                          return (pid, List.foldl List.append [] ints))
                                                                      papers;
                                                      rpc (saveAssignment ls)}/>
                    </body></xml>
                end
        in
            <xml>
              <li><a link={assignPapers ()}> Assign papers to people</a></li>
            </xml>
        end

    val linksForPc =
        let
            fun yourBids () =
                me <- getPcLogin;
                ps <- queryX (SELECT paper.Id, paper.{{M.paper}}, bid.Interest
                              FROM paper LEFT JOIN bid ON bid.Paper = paper.Id
                                AND bid.User = {[me.Id]})
                             (fn r => <xml><tr>
                               <td>{useMore (summarizePaper (r.Paper -- #Id))}</td>
                               <td><entry>
                                 <hidden{#Paper} value={show r.Paper.Id}/>
                                 <select{#Bid}>
                                   {useMore (Select.selectChar ((#"-", "No") :: (#"_", "Maybe") :: (#"+", "Yes") :: [])
                                                      r.Bid.Interest)}
                                 </select></entry></td>
                             </tr></xml>);
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
        (uid : userId) (fi : sql_from_items ([Paper = [Id = paperId] ++ paper] ++ tabs)) =
        sql_inner_join fi (sql_from_table [#Assignment] assignment)
                       (WHERE Paper.Id = Assignment.Paper AND Assignment.User = {[uid]})
end
