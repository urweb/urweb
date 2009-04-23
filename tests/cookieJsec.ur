table t : {Id : int}

cookie c : int

fun setter r =
    setCookie c (readError r.Id);
    return <xml>Done</xml>

fun writer () =
    ido <- getCookie c;
    case ido of
        None => error <xml>No cookie</xml>
      | Some id => dml (INSERT INTO t (Id) VALUES ({[id]}))

fun preWriter () = return <xml><body onload={onConnectFail (alert "RPC error")}>
  <button onclick={writer ()} value="Write to database"/>

  <a link={main ()}>Back</a>
</body></xml>

and main () = return <xml><body>
  <form>
    <textbox{#Id}/> <submit value="Get cookie" action={setter}/>
  </form>

  <form><submit action={preWriter} value="Prepare to write to database"/></form>
</body></xml>
