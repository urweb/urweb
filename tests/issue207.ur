val s1 = "𝕬" (* U+1D56C *)
val s2 : string = Json.fromJson "\"\\uD55C\\uD55C\\uD55C\\uD55C\""

val main x : transaction page =
    return <xml><body>
      <p>{[s1]} {[if x then s1 else s1]}</p>
      <p>{[s2]}</p>
    </body></xml>
