fun main () : transaction page =
    let
        val x = 1
        val y = "Hello"
        val z = 3.45
    in
        return <xml>{[x]}, {[y]}, {[z]}</xml>
    end
