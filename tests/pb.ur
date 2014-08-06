fun api_1 (pb:postBody) (nm:string) : transaction page =
  return <xml>Processing the request</xml>

fun api (pb:postBody) (v:int) (nm:string) : transaction page =
  case v of
      1 => api_1 pb nm
    | _ => error <xml>Version {[v]} is not supported</xml>
