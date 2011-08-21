datatype node
  = Node of
    { Label     : string
    , SubForest : list node
    }

fun getNode () : transaction node =
    return (Node { Label = "foo", SubForest = [] })

fun main () : transaction page = return <xml><body>
  <button onclick={
    n <- rpc (getNode ());
    case n of
        Node {Label = l, ...} => alert ("l = " ^ l)
  }/>
  </body></xml>
