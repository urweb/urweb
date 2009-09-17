fun one () = return 1

fun addEm n =
    if n = 0 then
        return 0
    else
        n1 <- rpc (one ());
        n2 <- addEm (n - 1);
        return (n1 + n2)

fun main () =
    s <- source 0;
    return <xml><body onload={n <- addEm 3; set s n}>
      <dyn signal={n <- signal s; return (txt n)}/>
    </body></xml>
