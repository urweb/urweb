sequence s
table t : { Id : int, Nam : option string, Data : blob, Desc : string, Typ : string }

fun view id =
    r <- oneRow (SELECT t.Data, t.Typ FROM t WHERE t.Id = {[id]});
    returnBlob r.T.Data (blessMime r.T.Typ)

fun save r =
    id <- nextval s;
    dml (INSERT INTO t (Id, Nam, Data, Desc, Typ)
         VALUES ({[id]}, {[fileName r.Data]}, {[fileData r.Data]}, {[r.Desc]}, {[fileMimeType r.Data]}));
    main ()

and main () =
    ls <- queryX (SELECT t.Id, t.Desc, octet_length(t.Data) AS Len FROM t ORDER BY t.Desc)
          (fn r => <xml><li><a link={view r.T.Id}>{[r.T.Desc]} ({[r.Len]})</a></li></xml>);
    return <xml><body>
      {ls}

      <br/>

      <form>
        <textbox{#Desc}/>
        <upload{#Data}/>
        <submit action={save}/>
      </form>
    </body></xml>
