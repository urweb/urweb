val current = return (Some "1")
fun resolve (_ : string) = return (Some "2")

fun checkDeps deps =
    u <- current;
    List.foldlM (fn s (good, errs) =>
                    v' <- resolve s;
                    case v' of
                        None =>
                        return (False, <xml>
                          {errs}
                          Unknown library path <tt>{[s]}</tt>.<br/>
                        </xml>)
                      | Some v' =>
                        b <- return True;
                        if b then
                            return (good, errs)
                        else
                            return (False, <xml>
                              {errs}
                              Access denied to <tt>{[s]}</tt>.<br/>
                            </xml>)) (True, <xml/>) deps

fun main () =
    p <- checkDeps ("a" :: "b" :: []);
    return <xml><body>
      {p.2}
    </body></xml>
