table t : { Id : int, B : string }
  PRIMARY KEY Id

table u : { Id : int, Link : int, C : string, D : option float }
  PRIMARY KEY Id,
  CONSTRAINT Link FOREIGN KEY Link REFERENCES t(Id)

fun main () =
    xml <- queryX (SELECT t.Id, t.B, u.Id, u.C, u.D
                   FROM t LEFT JOIN u ON t.Id = u.Link)
                  (fn r => <xml><tr>
                    <td>{[r.T.Id]}</td>
                    <td>{[r.T.B]}</td>
                    <td>{[r.U.Id]}</td>
                    <td>{[r.U.C]}</td>
                    <td>{[r.U.D]}</td>
                  </tr></xml>);
    return <xml><body>
      <table>{xml}</table>

      <form>Insert into t: <textbox{#Id} size={5}/> <textbox{#B} size={5}/>
        <submit action={addT}/></form>
      <form>
        Insert into u: <textbox{#Id} size={5}/> <textbox{#Link} size={5}/> <textbox{#C} size={5}/>
        <textbox{#D} size={5}/> <submit action={addU}/>
      </form>
    </body></xml>

and addT r =
    dml (INSERT INTO t (Id, B) VALUES ({[readError r.Id]}, {[r.B]}));
    main ()

and addU r =
    dml (INSERT INTO u (Id, Link, C, D) VALUES ({[readError r.Id]}, {[readError r.Link]}, {[r.C]}, {[readError r.D]}));
    main ()
