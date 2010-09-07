table t : {Id : int}
  PRIMARY KEY Id

fun doStuff () =
    dml (INSERT INTO t (Id) VALUES (0));
    o1 <- tryDml (INSERT INTO t (Id) VALUES (0));
    dml (INSERT INTO t (Id) VALUES (1));
    o2 <- tryDml (INSERT INTO t (Id) VALUES (2));
    dml (INSERT INTO t (Id) VALUES (3));
    o3 <- tryDml (INSERT INTO t (Id) VALUES (3));
    return <xml>{[o1]}; {[o2]}; {[o3]}</xml>

fun main () = return <xml><body>
  <form> <submit action={doStuff}/> </form>
</body></xml>
