sequence s
table t : { Id : int, Data : option blob, Typ : string }

fun view id =
    r <- oneRow (SELECT t.Data, t.Typ FROM t WHERE t.Id = {[id]});
    case r.T.Data of
        None => return <xml>This one's empty.</xml>
      | Some data => returnBlob data (blessMime r.T.Typ)

fun save r =
    id <- nextval s;
    dml (INSERT INTO t (Id, Data, Typ)
         VALUES ({[id]}, {[Some (fileData r.Data)]}, {[fileMimeType r.Data]}));
    main ()

and saveEmpty () =
    id <- nextval s;
    dml (INSERT INTO t (Id, Data, Typ)
         VALUES ({[id]}, {[None]}, "bogus"));
    main ()

and main () =
    ls <- queryX (SELECT t.Id FROM t)
          (fn r => <xml><li><a link={view r.T.Id}>{[r.T.Id]}</a></li></xml>);
    return <xml><body>
      {ls}

      <br/>

      <form>
        <upload{#Data}/>
        <submit action={save}/>
      </form>

      <form>
        <submit action={saveEmpty}/>
      </form>
    </body></xml>
