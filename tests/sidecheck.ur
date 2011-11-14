fun main () : transaction page =
    s <- source 0;
    n <- get s;
    set s (n + 1);
    n' <- get s;
    return <xml>{[n']}</xml>
