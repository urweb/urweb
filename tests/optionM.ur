fun main () : transaction page = return <xml>{[x <- Some 1;
                                               y <- Some 2;
                                               return (x + y)]}</xml>
