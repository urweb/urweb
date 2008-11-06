table t : { O : option int }

fun addNull () =
    dml (INSERT INTO t (O) VALUES (NULL));
    return <xml>Done</xml>

fun add3 () =
    dml (INSERT INTO t (O) VALUES ({Some 3}));
    return <xml>Done</xml>

fun addN r =
    dml (INSERT INTO t (O) VALUES ({Some (readError r.N)}));
    return <xml>Done</xml>

fun main () : transaction page =
    xml <- queryX (SELECT * FROM t)
                  (fn r => case r.T.O of
                               None => <xml>Nada<br/></xml>
                             | Some n => <xml>Num: {[n]}<br/></xml>);
    return <xml><body>
      {xml}

      <a link={addNull ()}>Add a null</a><br/>
      <a link={add3 ()}>Add a 3</a><br/>
      <form>
        Add <textbox{#N}/> <submit action={addN}/>
      </form>
    </body></xml>
