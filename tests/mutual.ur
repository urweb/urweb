datatype foo = A | B of bar
and bar = C | D of foo

val q = B (D A)

fun main () = return <xml>
  {case q of
       B (D A) => <xml>Good</xml>
     | _ => <xml>Bad</xml>}
</xml>
