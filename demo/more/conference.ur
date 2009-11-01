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
    table paper : ([Id = paperId, Document = blob] ++ paper)
                      PRIMARY KEY Id

    val checkLogin : transaction (option {Id : userId, Nam : string, Chair : bool, OnPc : bool})
    val getLogin : transaction {Id : userId, Nam : string, Chair : bool, OnPc : bool}
    val getPcLogin : transaction {Id : userId, Nam : string, Chair : bool}
    val summarizePaper : ctx ::: {Unit} -> [[Body] ~ ctx] => $paper -> xml ([Body] ++ ctx) [] []
end

signature OUTPUT = sig
    con paper :: {Type}
    type userId
    type paperId

    val linksForPc : xbody

    con yourPaperTables :: {{Type}}
    constraint [Paper] ~ yourPaperTables
    val joinYourPaper : tabs ::: {{Type}} -> paper ::: {Type}
        -> [[Paper] ~ tabs] => [[Paper] ~ yourPaperTables] => [tabs ~ yourPaperTables] => [[Id] ~ paper] =>
        sql_from_items ([Paper = [Id = paperId] ++ paper] ++ tabs)
        -> sql_from_items (yourPaperTables ++ [Paper = [Id = paperId] ++ paper] ++ tabs)
end

open Meta

functor Make(M : sig
                 con paper :: {(Type * Type)}
                 constraint [Id, Document, Authors] ~ paper
                 val paper : $(map meta paper)
                 val paperFolder : folder paper

                 con review :: {(Type * Type)}
                 constraint [Paper, User] ~ review
                 val review : $(map meta review)
                 val reviewFolder : folder review

                 val submissionDeadline : time
                 val summarizePaper : ctx ::: {Unit} -> [[Body] ~ ctx] => $(map fst paper) -> xml ([Body] ++ ctx) [] []

                 functor Make (M : INPUT where con paper = map fst paper)
                         : OUTPUT where con paper = map fst paper
                                  where con userId = M.userId
                                  where con paperId = M.paperId
             end) = struct

    table user : {Id : int, Nam : string, Password : string, Chair : bool, OnPc : bool}
          PRIMARY KEY Id,
          CONSTRAINT Nam UNIQUE Nam
    sequence userId

    con paper = [Id = int, Document = blob] ++ map fst M.paper
    table paper : paper
          PRIMARY KEY Id
    sequence paperId

    table authorship : {Paper : int, User : int}
          PRIMARY KEY (Paper, User),
          CONSTRAINT Paper FOREIGN KEY Paper REFERENCES paper(Id) ON DELETE CASCADE,
          CONSTRAINT User FOREIGN KEY User REFERENCES user(Id)

    con review = [Paper = int, User = int] ++ map fst M.review
    table review : review
          PRIMARY KEY (Paper, User),
          CONSTRAINT Paper FOREIGN KEY Paper REFERENCES paper(Id),
          CONSTRAINT User FOREIGN KEY User REFERENCES user(Id)
    sequence reviewId

    cookie login : {Id : int, Password : string}

    val checkLogin =
        r <- getCookie login;
        case r of
            None => return None
          | Some r =>
            oneOrNoRows1 (SELECT user.Id, user.Nam, user.Chair, user.OnPc
                          FROM user
                          WHERE user.Id = {[r.Id]}
                            AND user.Password = {[r.Password]})

    val getLogin =
        ro <- checkLogin;
        case ro of
            None => error <xml>You must be logged in to do that.</xml>
          | Some r => return r

    val getPcLogin =
        r <- getLogin;
        if r.OnPc then
            return (r -- #OnPc)
        else
            error <xml>You are not on the PC.</xml>

    structure O = M.Make(struct
                             val user = user
                             val paper = paper
                             val checkLogin = checkLogin
                             val getLogin = getLogin
                             val getPcLogin = getPcLogin
                             val summarizePaper = @@M.summarizePaper
                         end)

    val checkOnPc =
        r <- getLogin;
        if r.OnPc then
            return ()
        else
            error <xml>You aren't authorized to do that.</xml>

    fun checkPaper id =
        r <- getLogin;
        if r.OnPc then
            return ()
        else
            error <xml>You aren't authorized to see that paper.</xml>

    structure Users = BulkEdit.Make(struct
                                        con keyName = #Id
                                        val visible = {Nam = string "Name",
                                                       Chair = bool "Chair?",
                                                       OnPc = bool "On PC?"}

                                        val title = "Users"
                                        val isAllowed =
                                            me <- checkLogin;
                                            return (Option.isSome me)

                                        val t = user
                                    end)

    fun doRegister r =
        n <- oneRowE1 (SELECT COUNT( * ) AS N
                       FROM user
                       WHERE user.Nam = {[r.Nam]});
        if n > 0 then
            register (Some "Sorry; that username is taken.")
        else
            id <- nextval userId;
            dml (INSERT INTO user (Id, Nam, Password, Chair, OnPc)
                 VALUES ({[id]}, {[r.Nam]}, {[r.Password]}, FALSE, FALSE));
            setCookie login {Id = id, Password = r.Password};
            main ()

    and register msg = return <xml><body>
      <h1>Registering a New Account</h1>

      {case msg of
           None => <xml/>
         | Some msg => <xml><div>{[msg]}</div></xml>}

      <form><table>
        <tr> <th>Username:</th> <td><textbox{#Nam}/></td> </tr>
        <tr> <th>Password:</th> <td><password{#Password}/></td> </tr>
        <tr> <th><submit action={doRegister}/></th> </tr>
      </table></form>
    </body></xml>

    and signin r =
        ro <- oneOrNoRowsE1 (SELECT user.Id AS N
                             FROM user
                             WHERE user.Nam = {[r.Nam]}
                               AND user.Password = {[r.Password]});
        (case ro of
             None => return ()
           | Some id => setCookie login {Id = id, Password = r.Password});
        m <- main' ();
        return <xml><body>
          {case ro of
               None => <xml><div>Invalid username or password.</div></xml>
             | _ => <xml/>}

          {m}
        </body></xml>

    and main' () =
        me <- checkLogin;
        now <- now;
        return <xml><ul>
          {case me of
               None => <xml>
                 <li><a link={register None}>Register for access</a></li>
                 <li><b>Log in:</b> <form><table>
                   <tr> <th>Username:</th> <td><textbox{#Nam}/></td> </tr>
                   <tr> <th>Password:</th> <td><password{#Password}/></td> </tr>
                   <tr> <th><submit value="Log in" action={signin}/></th> </tr>
                 </table></form></li>
               </xml>
             | Some me => <xml>
               <div>Welcome, {[me.Nam]}!</div>

               {if me.Chair then
                    <xml><li><a link={Users.main ()}>Manage users</a></li></xml>
                else
                    <xml/>}

               {if me.OnPc then
                    <xml>
                      <li><a link={all ()}>All papers</a></li>
                      <li><a link={your ()}>Your papers</a></li>
                      {O.linksForPc}
                    </xml>
                else
                    <xml/>}

               {if now < M.submissionDeadline then
                    <xml><li><a link={submit ()}>Submit</a></li></xml>
                else
                    <xml/>}
             </xml>}
        </ul></xml>

    and main () =
        m <- main' ();
        return <xml><body>{m}</body></xml>

    and submit () =
        let
            fun doSubmit r =
                me <- getLogin;
                coauthors <- List.mapM (fn name => oneOrNoRowsE1 (SELECT user.Id AS N
                                                                  FROM user
                                                                  WHERE user.Nam = {[name.Nam]})) r.Authors;
                if List.exists Option.isNone coauthors then
                    error <xml>At least one of those coauthor usernames isn't registered.</xml>
                else
                    id <- nextval paperId;
                    dml (insert paper ({Id = sql_inject id, Document = sql_inject (fileData r.Document)}
                                           ++ ensql M.paper (r -- #Authors -- #Document) M.paperFolder));
                    List.app (fn uid =>
                                 case uid of
                                     None => error <xml>Impossible empty uid!</xml>
                                   | Some uid => dml (INSERT INTO authorship (Paper, User)
                                                      VALUES ({[id]}, {[uid]})))
                             (Some me.Id :: coauthors);
                    return <xml><body>
                      Thanks for submitting!
                    </body></xml>
        in
            me <- getLogin;
            numAuthors <- Dnat.zero;

            return <xml><body>
              <h1>Submit a Paper</h1>
              
              <form>
                <b>Author:</b> {[me.Nam]}<br/>
                <subforms{#Authors}>
                  {Dnat.render <xml><entry><b>Author:</b> <textbox{#Nam}/><br/></entry></xml> numAuthors}
                </subforms>
                <button value="Add author" onclick={Dnat.inc numAuthors}/><br/>
                <button value="Remove author" onclick={Dnat.dec numAuthors}/><br/>
                <br/>

                {useMore (allWidgets M.paper M.paperFolder)}
                <b>Paper:</b> <upload{#Document}/><br/>
                <submit value="Submit" action={doSubmit}/>
              </form>
            </body></xml>
        end

    and listPapers [tabs] [[Paper] ~ tabs] (q : sql_query ([Paper = [Id = int] ++ map fst M.paper] ++ tabs) []) =
        checkOnPc;
        ps <- queryX q
                     (fn r => <xml><li><a link={one r.Paper.Id}>{M.summarizePaper (r.Paper -- #Id)}</a></li></xml>);
        return <xml><body>
          <h1>All Papers</h1>
          
          <ul>
            {ps}
          </ul>
        </body></xml>

    and all () =
        checkOnPc;
        listPapers (SELECT paper.Id, paper.{{map fst M.paper}} FROM paper)

    and your () =
        me <- getLogin;
        listPapers (sql_query {Rows = sql_query1 {Distinct = False,
                                                  From = O.joinYourPaper (sql_from_table [#Paper] paper),
                                                  Where = (WHERE TRUE),
                                                  GroupBy = sql_subset_all [_],
                                                  Having = (WHERE TRUE),
                                                  SelectFields = sql_subset [[Paper = ([Id = _] ++ map fst M.paper, _)]
                                                                                 ++ map (fn ts => ([], ts))
                                                                                            O.yourPaperTables],
                                                  SelectExps = {}},
                               OrderBy = sql_order_by_Nil [_],
                               Limit = sql_no_limit,
                               Offset = sql_no_offset})

    and one id =
        let
            fun newReview r =
                me <- getLogin;
                checkPaper id;
                dml (insert review ({Paper = sql_inject id, User = sql_inject me.Id}
                                        ++ ensql M.review r M.reviewFolder));
                one id

            fun saveReview r =
                me <- getLogin;
                checkPaper id;
                dml (update [map fst M.review] ! (ensql M.review r M.reviewFolder)
                            review (WHERE T.Paper = {[id]} AND T.User = {[me.Id]}));
                one id
        in
            me <- getLogin;
            checkPaper id;
            ro <- oneOrNoRows (SELECT paper.{{map fst M.paper}}, octet_length(paper.Document) AS N
                               FROM paper
                               WHERE paper.Id = {[id]});
            authors <- queryX (SELECT user.Nam
                               FROM authorship
                                 JOIN user ON authorship.User = user.Id
                               WHERE authorship.Paper = {[id]})
                              (fn r => <xml><li>{[r.User.Nam]}</li></xml>);
            myReview <- oneOrNoRows1 (SELECT review.{{map fst M.review}}
                                      FROM review
                                      WHERE review.User = {[me.Id]}
                                        AND review.Paper = {[id]});
            case ro of
                None => error <xml>Paper not found!</xml>
              | Some r => return <xml><body>
                <h1>Paper #{[id]}</h1>

                <h3>Authors:</h3>
                <ul>
                  {authors}
                </ul>

                {allContent M.paper r.Paper M.paperFolder}<br/>

                {if r.N = 0 then
                     <xml><div>No paper uploaded yet.</div></xml>
                 else
                     <xml><a link={download id}>Download paper</a> ({[r.N]} bytes)</xml>}

                <hr/>

                {case myReview of
                     None => <xml>
                       <h2>Add Your Review</h2>
                       
                       <form>
                         {allWidgets M.review M.reviewFolder}
                         <submit value="Add" action={newReview}/>
                       </form>
                     </xml>
                   | Some myReview => <xml>
                     <h2>Edit Your Review</h2>

                     <form>
                       {allPopulated M.review myReview M.reviewFolder}
                       <submit value="Save" action={saveReview}/>
                     </form>
                   </xml>}
              </body></xml>
        end

    and download id =
        checkPaper id;
        ro <- oneOrNoRows (SELECT paper.Document
                           FROM paper
                           WHERE paper.Id = {[id]});
        case ro of
            None => error <xml>Paper not found!</xml>
          | Some r => returnBlob r.Paper.Document (blessMime "application/pdf")

end
