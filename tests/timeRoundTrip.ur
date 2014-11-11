fun main () : transaction page =
    t <- now;
    return <xml>{[readError (show t) : time]}</xml>
