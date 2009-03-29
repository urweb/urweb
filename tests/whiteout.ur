table t : { Chan : option (channel unit) }

fun main () : transaction page =
    ch <- channel;
    dml (INSERT INTO t (Chan) VALUES ({[Some ch]}));
    return <xml><body>Did it.</body></xml>
