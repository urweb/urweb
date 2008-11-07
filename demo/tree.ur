sequence s
table t : { Id : int, Parent : option int, Nam : string }

open TreeFun.Make(struct
                      val tab = t
                  end)

fun row r = <xml>
  #{[r.Id]}: {[r.Nam]} <a link={del r.Id}>[Delete]</a>

  <form>
    Add child: <textbox{#Nam}/> <submit action={add (Some r.Id)}/>
  </form>
</xml>

and main () =
    xml <- tree row None;
    return <xml><body>
      {xml}

      <form>
        Add a top-level node: <textbox{#Nam}/> <submit action={add None}/>
      </form>
    </body></xml>

and add parent r =
    id <- nextval s;
    dml (INSERT INTO t (Id, Parent, Nam) VALUES ({[id]}, {[parent]}, {[r.Nam]}));
    main ()

and del id =
    dml (DELETE FROM t WHERE Id = {[id]});
    main ()
