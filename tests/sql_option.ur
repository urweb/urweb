table t : { O : option int }

fun addNull () =
    dml (INSERT INTO t (O) VALUES (NULL));
    return <xml>Done</xml>

(*fun add42 () =
    dml (INSERT INTO t (O) VALUES (42));
    return <xml>Done</xml>*)

fun main () : transaction page =
    xml <- queryX (SELECT * FROM t)
                  (fn r => case r.T.O of
                               None => <xml>Nada<br/></xml>
                             | Some n => <xml>Num: {[n]}<br/></xml>);
    return <xml><body>
      {xml}

      <a link={addNull ()}>Add a null</a><br/>
    </body></xml>

(*      <a link={add42 ()}>Add a 42</a><br/>*)
