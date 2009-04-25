sequence s
table t : { Id : int, Nam : option string, Data : blob, Desc : string }

fun save r =
    if numFiles r.Data <> 1 then
        error <xml>Please submit exactly one file.</xml>
    else
        let
            val f = fileNum r.Data 0
        in
            id <- nextval s;
            dml (INSERT INTO t (Id, Nam, Data, Desc) VALUES ({[id]}, {[fileName f]}, {[fileData f]}, {[r.Desc]}));
            main ()
        end

and main () = return <xml><body>
  <form>
    <textbox{#Desc}/>
    <upload{#Data}/>
    <submit action={save}/>
  </form>
</body></xml>
