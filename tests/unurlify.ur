datatype list t = Nil | Cons of t * list t

fun handler (ls : list bool) = return <xml/>

datatype wlist = WNil | Empty | WCons of bool * wlist

fun whandler' (ls : wlist) =
    case ls of
        WNil => <xml>Nil</xml>
      | Empty => <xml>Empty</xml>
      | WCons (x, ls') => <xml>{[x]} :: {whandler' ls'}</xml>

fun whandler ls = return (whandler' ls)

fun main () : transaction page = return <xml><body>
  <a link={handler Nil}>!</a><br/>
  <a link={whandler WNil}>Nil</a><br/>
  <a link={whandler Empty}>Empty</a><br/>
  <a link={whandler (WCons (True, WCons (False, Empty)))}>True :: False :: Empty</a><br/>
</body></xml>
