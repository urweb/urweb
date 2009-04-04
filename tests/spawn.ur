table t : {A : int, Ch : channel string}

fun listener n ch =
    s <- recv ch;
    alert (show n ^ ": " ^ s);
    listener n ch

fun speak id msg =
    r <- oneRow (SELECT t.Ch FROM t WHERE t.A = {[id]});
    send r.T.Ch msg

fun main () : transaction page =
    ch1 <- channel;
    dml (INSERT INTO t (A, Ch) VALUES (1, {[ch1]}));
    ch2 <- channel;
    dml (INSERT INTO t (A, Ch) VALUES (2, {[ch2]}));

    s1 <- source "";
    s2 <- source "";

    return <xml><body onload={spawn (listener 1 ch1); spawn (listener 2 ch2)}>
      1: <ctextbox source={s1}/><button onclick={msg <- get s1; speak 1 msg}/><br/>
      2: <ctextbox source={s2}/><button onclick={msg <- get s2; speak 2 msg}/>
    </body></xml>
