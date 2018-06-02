datatype bugged = Nothing | Something of int
datatype myDt = One | Two
type myRecord = {Bugged: bugged
               , MyDt : myDt}

fun rpcTarget (t: myRecord) = return ()

val good = {Bugged = Something 4, MyDt = One}
val bad = {Bugged = Nothing, MyDt = One}

fun main () : transaction page = return <xml>
  <body>
    <button onclick={fn _ => rpc (rpcTarget good)}>rpc with good</button>
    <button onclick={fn _ => rpc (rpcTarget bad)}>rpc with bad</button>
  </body>
</xml>
