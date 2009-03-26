sequence s
table t : { Id : int, Title : string, Chan : option (channel string) }

fun list () =
    queryX (SELECT * FROM t)
    (fn r => <xml><tr>
      <td>{[r.T.Id]}</td> <td>{[r.T.Title]}</td>
      <td><a link={delete r.T.Id}>[delete]</a></td>
    </tr></xml>)

and delete id =
    dml (DELETE FROM t WHERE Id = {[id]});
    main ()

and main () : transaction page =
    let
        fun create r =
            id <- nextval s;
            dml (INSERT INTO t (Id, Title, Chan) VALUES ({[id]}, {[r.Title]}, NULL));
            main ()
    in
        ls <- list ();
        return <xml><body>
          <table>
            <tr> <th>ID</th> <th>Title</th> </tr>
            {ls}
          </table>
          
          <h1>New Channel</h1>
          
          <form>
            Title: <textbox{#Title}/><br/>
            <submit action={create}/>
          </form>
        </body></xml>
    end
