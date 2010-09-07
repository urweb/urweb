fun main n =
  case n of
      0 => error <xml>Zero is bad!</xml>
    | _ => return <xml/>
