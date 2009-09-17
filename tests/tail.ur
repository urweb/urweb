fun one () = return 1

fun addEm n =
    if n = 0 then
        return 0
    else
        n1 <- rpc (one ());
        n2 <- addEm (n - 1);
        return (n1 + n2)

fun addEm' n acc =
    if n = 0 then
        return acc
    else
        n1 <- rpc (one ());
        addEm' (n - 1) (n1 + acc)

fun main () =
    s <- source 0;
    s' <- source 0;
    return <xml><body onload={n <- addEm 3; set s n; n <- addEm' 4 0; set s' n; alert "Welcome!"}>
      <dyn signal={n <- signal s; return (txt n)}/>
      <dyn signal={n <- signal s'; return (txt n)}/>
    </body></xml>
