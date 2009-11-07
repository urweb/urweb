val decision = {Nam = "Decision",
                Initialize = None,
                Show = fn bo => cdata (case bo of
                                           None => "?"
                                         | Some True => "Accept"
                                         | Some False => "Reject"),
                Inject = _}

functor Make(M : sig
                 con paperOther :: {Type}
                 constraint [Id, Decision] ~ paperOther
                 include Conference.INPUT
                         where con paper = [Decision = option bool] ++ paperOther

                 val status : ctx ::: {Unit} -> [[Body] ~ ctx] => $([Id = paperId] ++ paperOther)
                                                                  -> transaction (xml ([Body] ++ ctx) [] [])
             end) = struct
    open M

    val linksForChair =
        let
            fun makeDecisions () =
                ps <- queryX' (SELECT paper.Id, paper.Decision, paper.{{M.paperOther}}
                               FROM paper
                               ORDER BY paper.Id)
                              (fn r => st <- status (r.Paper -- #Decision);
                                  return <xml><tr>
                                    <td>{useMore (summarizePaper (r.Paper -- #Id))}</td>
                                    <td>{useMore st}</td>
                                    <td><entry>
                                      <hidden{#Paper} value={show r.Paper.Id}/>
                                      <select{#Decision}>
                                        <option selected={r.Paper.Decision = None}>?</option>
                                        <option selected={r.Paper.Decision = Some True}>Accept</option>
                                        <option selected={r.Paper.Decision = Some False}>Reject</option>
                                    </select></entry></td>
                                  </tr></xml>);
                return <xml><body>
                  <h1>Make acceptance decisions</h1>

                  <form>
                    <subforms{#Papers}>
                      <table>
                        <tr> <th>Paper</th> <th>Status</th> <th>Decision</th> </tr>
                        {ps}
                      </table>
                    </subforms>
                    <submit value="Save" action={saveDecisions}/>
                  </form>
                </body></xml>

            and saveDecisions r =
                List.app (fn {Paper = pid, Decision = dec} =>
                             dml (UPDATE paper
                                  SET Decision = {[case dec of
                                                       "?" => None
                                                     | "Accept" => Some True
                                                     | "Reject" => Some False
                                                     | _ => error <xml>Invalid decision code</xml>]}
                                  WHERE Id = {[readError pid]})) r.Papers;
                makeDecisions ()
        in
            <xml>
              <li><a link={makeDecisions ()}>Make acceptance decisions</a></li>
            </xml>
        end

    val linksForPc = <xml/>

    con yourPaperTables = []
    constraint [Paper] ~ yourPaperTables
    fun joinYourPaper [tabs] [paper] [[Paper] ~ tabs] [[Paper] ~ _] [tabs ~ yourPaperTables] [[Id] ~ paper]
        uid (fi : sql_from_items ([Paper = [Id = paperId] ++ paper] ++ tabs)) = fi
end
