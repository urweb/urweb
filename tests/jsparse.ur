fun main () =
    s <- source "13";
    return <xml><body>
      <ctextbox source={s}/>
      <dyn signal={v <- signal s; return (case read v : option int of
                                              None => <xml>None</xml>
                                            | Some n => <xml>Some {[n]}</xml>)}/>
      </body></xml>
