sequence s
table t : { Id : int, S1 : string, S2:string, S3:string, S4:string }

fun each (n : int, (f : unit -> transaction unit)) = if n > 0 then f (); each ((n-1),f) else return ()

fun fill () =
    dml (DELETE FROM t WHERE 1=1);
    each (1,( fn () =>
      (nv <- nextval s;
      (dml (INSERT INTO t (Id, S1, S2, S3, S4) VALUES ({[nv]}, {["S1"]}, {["S2"]}, {["S3"]}, {["S4"]}))))
    ));
    return <xml>done</xml>

fun main () = return <xml><body>
    <form><submit action={fill} value="fill"/></form>
</body></xml>
