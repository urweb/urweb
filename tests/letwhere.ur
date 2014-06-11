fun main () : transaction page =
    let
        return <xml>Hi {[alice]} and {[bob]}!</xml>
    where
        val alice = "Alice"
        val bob = "Bob"
    end
