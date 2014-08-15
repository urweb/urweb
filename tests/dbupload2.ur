table t : { Id : int, Blob : blob, MimeType : string }
sequence s

fun getImage id : transaction page =
    r <- oneRow1 (SELECT t.Blob, t.MimeType
                  FROM t
                  WHERE t.Id = {[id]});
    returnBlob r.Blob (blessMime r.MimeType)

fun handle (r : {File:file, Param:string}) =
    id <- nextval s;
    dml (INSERT INTO t (Id, Blob, MimeType)
         VALUES ({[id]}, {[fileData r.File]}, {[fileMimeType r.File]}));
    debug ("Text is " ^ r.Param);
    main ()

and main () : transaction page =
    x <- queryX1 (SELECT t.Id FROM t)
                 (fn r => <xml><img src={url (getImage r.Id)}/>
</xml>);
    return <xml><body>
      <form>
      <upload{#File}/>
      <textbox{#Param} value="text"/>
      <submit action={handle}/>
      </form>
      <hr/>
      {x}
    </body></xml>
