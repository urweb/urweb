table t : { Id : int, Nam : string, Parent : option int }
  PRIMARY KEY Id,
  CONSTRAINT Nam UNIQUE Nam,
  CONSTRAINT Id CHECK Id >= 0,
  CONSTRAINT Parent FOREIGN KEY Parent REFERENCES t(Id)

fun main () =
    list <- queryX (SELECT * FROM t)
            (fn r => <xml><tr>
              <td>{[r.T.Id]}</td>
              <td>{[r.T.Nam]}</td>
              <td>{case r.T.Parent of
                       None => <xml>NULL</xml>
                     | Some id => <xml>{[id]}</xml>}</td>
            </tr></xml>);
    return <xml><body>
      <table>
        <tr> <th>Id</th> <th>Name</th> <th>Parent</th> </tr>
        {list}
      </table>

      <form>
        <table>
          <tr> <th>Id:</th> <td><textbox{#Id}/></td> </tr>
          <tr> <th>Name:</th> <td><textbox{#Nam}/></td> </tr>
          <tr> <th>Parent:</th> <td><textbox{#Parent}/></td> </tr>
          <tr> <th/> <td><submit action={add}/></td> </tr>
        </table>
      </form>
    </body></xml>

and add r =
    dml (INSERT INTO t (Id, Nam, Parent)
           VALUES ({[readError r.Id]}, {[r.Nam]},
               {[case r.Parent of
                   "" => None
                 | s => Some (readError s)]}));
    main ()
