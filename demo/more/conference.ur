con meta = fn (db :: Type, widget :: Type) =>
              {Show : db -> xbody,
               Widget : nm :: Name -> xml form [] [nm = widget],
               WidgetPopulated : nm :: Name -> db -> xml form [] [nm = widget],
               Parse : widget -> db,
               Inject : sql_injectable db}

fun default [t] (sh : show t) (rd : read t) (inj : sql_injectable t) : meta (t, string) =
    {Show = txt,
     Widget = fn [nm :: Name] => <xml><textbox{nm}/></xml>,
     WidgetPopulated = fn [nm :: Name] n =>
                          <xml><textbox{nm} value={show n}/></xml>,
     Parse = readError,
     Inject = _}

val int = default
val float = default
val string = default
val bool = {Show = txt,
            Widget = fn [nm :: Name] => <xml><checkbox{nm}/></xml>,
            WidgetPopulated = fn [nm :: Name] b =>
                                 <xml><checkbox{nm} checked={b}/></xml>,
            Parse = fn x => x,
            Inject = _}

functor Make(M : sig
                 con paper :: {(Type * Type)}
                 constraint [Id, Title] ~ paper
                 val paper : $(map meta paper)

                 con review :: {(Type * Type)}
                 constraint [Paper, User] ~ review
                 val review : $(map meta review)
             end) = struct

    table user : {Id : int, Nam : string, Password : string, Chair : bool, OnPc : bool}
          PRIMARY KEY Id,
          CONSTRAINT Nam UNIQUE Nam
    sequence userId

    con paper = [Id = int, Title = string] ++ map fst M.paper
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

    fun checkLogin () =
        r <- getCookie login;
        case r of
            None => return None
          | Some r =>
            oneOrNoRows1 (SELECT user.Id, user.Nam, user.Chair, user.OnPc
                          FROM user
                          WHERE user.Id = {[r.Id]}
                            AND user.Password = {[r.Password]})

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

    and main () =
        me <- checkLogin ();
        return <xml><body>
          {case me of
               None => <xml><li><a link={register None}>Register for access</a></li></xml>
             | Some {Nam = name, ...} => <xml>Welcome, {[name]}!</xml>}
        </body></xml>

end
