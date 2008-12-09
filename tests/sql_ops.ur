table t : { A : int, B : float }

val q = (SELECT t.A + t.A AS X, t.B * t.B AS Y FROM t)

fun main () : transaction page =
    xml <- queryX q (fn r => <xml>{[r.X]}, {[r.Y]}<br/></xml>);
    return <xml><body>{xml}</body></xml>

