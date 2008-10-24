table t : { X : int }

fun main () : transaction page =
    v <- query (SELECT * FROM t)
         (fn r (_ : int) => return (error <xml>Shot down!</xml>))
         0;
    return <xml>Result: {[v]}</xml>
