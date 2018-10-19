fun alerts n =
    if n <= 0 then
        return ()
    else
        (alert ("Alert #" ^ show n);
         alerts (n - 1))

cookie uhoh : serialized xbody

fun main () : transaction page =
    setCookie uhoh {Value = serialize <xml><active code={alerts 3; return <xml>Yay!</xml>}/></xml>,
                    Expires = None,
                    Secure = False};
    return <xml></xml>
