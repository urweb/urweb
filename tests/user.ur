table user : {A : int}

fun main () =
    r <- oneRow (SELECT COUNT( * ) AS N FROM user);
    return <xml>{[r.N]}</xml>
