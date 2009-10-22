open Meta

functor Make(M : sig
                 con paper :: {(Type * Type)}
                 constraint [Id, Document] ~ paper
                 val paper : $(map meta paper)
                 val paperFolder : folder paper

                 con review :: {(Type * Type)}
                 constraint [Paper, User] ~ review
                 val review : $(map meta review)

                 val submissionDeadline : time
             end) = struct

    table user : {Id : int, Nam : string, Password : string, Chair : bool, OnPc : bool}
          PRIMARY KEY Id,
          CONSTRAINT Nam UNIQUE Nam
    sequence userId

    con paper = [Id = int, Document = blob] ++ map fst M.paper
    table paper : paper
          PRIMARY KEY Id
    sequence paperId

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
            fun doSubmit r = return <xml><body>
              MIME type: {[fileMimeType r.Document]}<br/>
              Length: {[blobSize (fileData r.Document)]}
            </body></xml>
        in
            return <xml><body>
              <h1>Submit a Paper</h1>
              
              <form>
                {allWidgets M.paper M.paperFolder}
                <b>Paper:</b> <upload{#Document}/><br/>
                <submit value="Submit" action={doSubmit}/>
              </form>
            </body></xml>
        end

end
