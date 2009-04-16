datatype list t = Nil | Cons of t * list t

table t : {Id : int, A : string}
  PRIMARY KEY Id

fun allRows () =
    query (SELECT * FROM t)
    (fn r acc => return (Cons ((r.T.Id, r.T.A), acc)))
    Nil

fun doBatch ls =
    case ls of
        Nil => return ()
      | Cons ((id, a), ls') =>
        dml (INSERT INTO t (Id, A) VALUES ({[id]}, {[a]}));
        doBatch ls'

fun del id =
    dml (DELETE FROM t WHERE t.Id = {[id]})

fun show withDel lss =
    let
        fun show' ls =
            case ls of
                Nil => <xml/>
              | Cons ((id, a), ls) => <xml>
                <tr><td>{[id]}</td> <td>{[a]}</td> {if withDel then
                                                        <xml><td><button value="Delete" onclick={del id}/></td></xml>
                                                    else
                                                        <xml/>} </tr>
                {show' ls}
              </xml>
    in
        <xml><dyn signal={ls <- signal lss; return <xml><table>
          <tr> <th>Id</th> <th>A</th> </tr>
          {show' ls}
        </table></xml>}/></xml>
    end

fun action () =
    lss <- source Nil;
    batched <- source Nil;

    id <- source "";
    a <- source "";

    let
        fun add () =
            id <- get id;
            a <- get a;
            ls <- get batched;

            set batched (Cons ((readError id, a), ls))

        fun exec () =
            ls <- get batched;

            doBatch ls;
            set batched Nil
    in
        return <xml><body>
          <h2>Rows</h2>

          {show True lss}

          <button value="Update" onclick={ls <- allRows (); set lss ls}/><br/>
          <br/>

          <h2>Batch new rows to add</h2>

          <table>
            <tr> <th>Id:</th> <td><ctextbox source={id}/></td> </tr>
            <tr> <th>A:</th> <td><ctextbox source={a}/></td> </tr>
            <tr> <th/> <td><button value="Batch it" onclick={add ()}/></td> </tr>
          </table>

          <h2>Already batched:</h2>
          {show False batched}
          <button value="Execute" onclick={exec ()}/>
        </body></xml>
    end

fun main () = return <xml><body>
  <form><submit value="Begin demo" action={action}/></form>
</body></xml>
