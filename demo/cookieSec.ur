cookie username : string

table lastVisit : { User : string, When : time }
  PRIMARY KEY User

fun main () =
    userO <- getCookie username;

    list <- queryX (SELECT * FROM lastVisit)
                   (fn r => <xml><tr><td>{[r.LastVisit.User]}</td> <td>{[r.LastVisit.When]}</td></tr></xml>);

    return <xml><body>
      Cookie: {[userO]}<br/>

      <table>
        <tr><th>User</th> <th>Last Visit</th></tr>
        {list}
      </table>

      <h2>Set cookie value</h2>
      <form><textbox{#User}/> <submit action={set}/></form>

      <h2>Record your visit</h2>
      <form><submit action={imHere}/></form>
    </body></xml>

and set r =
    setCookie username {Value = r.User, Expires = None, Secure = False};
    main ()

and imHere () =
    userO <- getCookie username;
    case userO of
        None => return <xml>You don't have a cookie set!</xml>
      | Some user =>
        dml (DELETE FROM lastVisit WHERE User = {[user]});
        dml (INSERT INTO lastVisit (User, When) VALUES ({[user]}, CURRENT_TIMESTAMP));
        main ()

