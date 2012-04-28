fun main n =
  case n of
      0 => error <xml>Zero is bad! <button value="Capische?" onclick={alert "GOOD!"}/></xml>
    | _ => return <xml/>
