val r = {A = 1, B = "Hi", C = 0.0}
val rA = r -- #A
val rB = r --- [A = _, C = _]

fun main () : transaction page = return <xml>
        {cdata rA.B}, {cdata rB.B}
</xml>
