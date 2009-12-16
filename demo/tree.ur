sequence s
table t : { Id : int, Parent : option int, Nam : string }
  PRIMARY KEY Id,
  CONSTRAINT F FOREIGN KEY Parent REFERENCES t (Id) ON DELETE CASCADE

open TreeFun.Make(struct
                      con id = #Id
                      con parent = #Parent
                      val tab = t
                  end)

fun row r = <xml>
  #{[r.Id]}: {[r.Nam]} <form><submit action={del r.Id} value="Delete"/></form>

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

and del id () =
    dml (DELETE FROM t WHERE Id = {[id]});
    main ()
