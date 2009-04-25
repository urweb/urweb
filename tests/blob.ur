sequence s
table t : { Id : int, Nam : option string, Data : blob, Desc : string }

fun save r =
    id <- nextval s;
    dml (INSERT INTO t (Id, Nam, Data, Desc) VALUES ({[id]}, {[fileName r.Data]}, {[fileData r.Data]}, {[r.Desc]}));
    main ()

and main () = return <xml><body>
  <form>
    <textbox{#Desc}/>
    <upload{#Data}/>
    <submit action={save}/>
  </form>
</body></xml>
