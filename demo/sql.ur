table t : { A : int, B : float, C : string, D : bool }
  PRIMARY KEY A

fun list () =
    rows <- queryX (SELECT * FROM t)
            (fn row => <xml><tr>
              <td>{[row.T.A]}</td> <td>{[row.T.B]}</td> <td>{[row.T.C]}</td> <td>{[row.T.D]}</td>
              <td><form><submit action={delete row.T.A} value="Delete"/></form></td>
            </tr></xml>);
    return <xml>
      <table border=1>
        <tr> <th>A</th> <th>B</th> <th>C</th> <th>D</th> </tr>
        {rows}
      </table>

      <br/><hr/><br/>

      <form>
        <table>
          <tr> <th>A:</th> <td><textbox{#A}/></td> </tr>
          <tr> <th>B:</th> <td><textbox{#B}/></td> </tr>
          <tr> <th>C:</th> <td><textbox{#C}/></td> </tr>
          <tr> <th>D:</th> <td><checkbox{#D}/></td> </tr>
          <tr> <th/> <td><submit action={add} value="Add Row"/></td> </tr>
        </table>
      </form>
    </xml>

and add r =
    dml (INSERT INTO t (A, B, C, D)
         VALUES ({[readError r.A]}, {[readError r.B]}, {[r.C]}, {[r.D]}));
    xml <- list ();
    return <xml><body>
      <p>Row added.</p>

      {xml}
    </body></xml>

and delete a () =
    dml (DELETE FROM t
         WHERE t.A = {[a]});
    xml <- list ();
    return <xml><body>
      <p>Row deleted.</p>

      {xml}
    </body></xml>

fun main () =
    xml <- list ();
    return <xml><body>
      {xml}
    </body></xml>
