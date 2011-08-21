fun showList l = case l of
    [] => "[]"
  | h :: t => strcat (strcat (show h) " :: ") (showList t)

fun rpcFunc l : transaction string =
  case l of h :: _ => return (showList h) | [] => return "[]"

fun main () : transaction page = return <xml><body>
  <button onclick={
    s <- rpc (rpcFunc  (("foo" :: []) :: []));
    alert s
  }/>
  </body></xml>
