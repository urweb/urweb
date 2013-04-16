fun isBeppo (s : string) : transaction string =
    case s of
        "Beppo" => return "Yup, that's him!"
      | "Mephisto" => error <xml>Great googely moogely!</xml>
      | _ => return "Who's that?"

fun listOf (n : int) =
    if n < 0 then
        error <xml>Negative!</xml>
    else if n = 0 then
        return []
    else
        ls <- listOf (n - 1);
        return (n :: ls)

fun length ls =
    case ls of
        [] => 0
      | _ :: ls' => 1 + length ls'

fun main () : transaction page =
    s <- source "";
    ns <- source "";
    return <xml><body>
      <ctextbox source={s}/>
      <button value="rpc" onclick={fn _ => v <- get s;
                                      r <- rpc (isBeppo v);
                                      alert r}/>
      <button value="tryRpc" onclick={fn _ => v <- get s;
                                         r <- tryRpc (isBeppo v);
                                         case r of
                                             None => alert "Faaaaaailure."
                                           | Some r => alert ("Success: " ^ r)}/>

      <hr/>

      <ctextbox source={ns}/>
      <button value="rpc" onclick={fn _ => v <- get ns;
                                      r <- rpc (listOf (readError v));
                                      alert (show (length r))}/>
      <button value="tryRpc" onclick={fn _ => v <- get ns;
                                         r <- tryRpc (listOf (readError v));
                                         case r of
                                             None => alert "Faaaaaailure."
                                           | Some r => alert ("Success: " ^ show (length r))}/>
    </body></xml>
