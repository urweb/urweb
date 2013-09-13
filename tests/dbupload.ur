table t : { Id : int, Blob : blob, MimeType : string }
sequence s

fun getImage id : transaction page =
    r <- oneRow1 (SELECT t.Blob, t.MimeType
                  FROM t
                  WHERE t.Id = {[id]});
    returnBlob r.Blob (blessMime r.MimeType)

fun main () : transaction page =
    let
        fun handle r =
            id <- nextval s;
            dml (INSERT INTO t (Id, Blob, MimeType)
                 VALUES ({[id]}, {[fileData r.File]}, {[fileMimeType r.File]}));
            main ()
    in
        x <- queryX1 (SELECT t.Id FROM t)
                     (fn r => <xml><img src={url (getImage r.Id)}/><br/></xml>);
        return <xml><body>
          <form><upload{#File}/> <submit action={handle}/></form>
          <hr/>
          {x}
        </body></xml>
    end
