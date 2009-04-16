table t : {Id : int}

cookie c : int

fun setter r =
    setCookie c (readError r.Id);
    return <xml>Done</xml>

fun writer () =
    ido <- getCookie c;
    case ido of
        None => error <xml>No cookie</xml>
      | Some id => dml (INSERT INTO t (Id) VALUES ({[id]}));
                   return <xml>Done</xml>

fun main () = return <xml><body>
  <form>
    <textbox{#Id}/> <submit value="Get cookie" action={setter}/>
  </form>

  <form>
    <submit value="Write to database" action={writer}/>
  </form>
</body></xml>
