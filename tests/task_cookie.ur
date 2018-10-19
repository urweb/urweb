cookie myCookie: {Value: string}

fun main (): transaction page = return <xml></xml>

task initialize = fn () => 
  c <- getCookie myCookie;
  case c of
      None => debug "No cookie"
    | Some {Value = v} => debug ("Cookie value: " ^ v)
