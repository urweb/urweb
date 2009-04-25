sequence s
table t : { Id : int, Nam : option string, Data : blob, Desc : string, Typ : string }

fun save r =
    id <- nextval s;
    dml (INSERT INTO t (Id, Nam, Data, Desc, Typ)
         VALUES ({[id]}, {[fileName r.Data]}, {[fileData r.Data]}, {[r.Desc]}, {[fileMimeType r.Data]}));
    main ()

and main () = return <xml><body>
  <form>
    <textbox{#Desc}/>
    <upload{#Data}/>
    <submit action={save}/>
  </form>
</body></xml>
