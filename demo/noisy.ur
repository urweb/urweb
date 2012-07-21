datatype list t = Nil | Cons of t * list t

table t : { Id : int, A : string }
  PRIMARY KEY Id

fun add id s =
    dml (INSERT INTO t (Id, A) VALUES ({[id]}, {[s]}))

fun del id =
    dml (DELETE FROM t WHERE t.Id = {[id]})

fun lookup id =
    ro <- oneOrNoRows (SELECT t.A FROM t WHERE t.Id = {[id]});
    case ro of
        None => return None
      | Some r => return (Some r.T.A)

fun check ls =
    case ls of
        Nil => return ()
      | Cons (id, ls') =>
        ao <- rpc (lookup id);
        alert (case ao of
                   None => "Nada"
                 | Some a => a);
        check ls'

fun main () =
    idAdd <- source "";
    aAdd <- source "";

    idDel <- source "";

    return <xml><body>
      <button value="Check values of 1, 2, and 3" onclick={fn _ => check (Cons (1, Cons (2, Cons (3, Nil))))}/><br/>
      <br/>
      <button value="Add" onclick={fn _ => id <- get idAdd; a <- get aAdd; rpc (add (readError id) a)}/>
      <ctextbox source={idAdd}/>
      <ctextbox source={aAdd}/><br/>
      <br/>
      <button value="Delete" onclick={fn _ => id <- get idDel; rpc (del (readError id))}/>
      <ctextbox source={idDel}/>
    </body></xml>
