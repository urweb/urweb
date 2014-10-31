table foo01 : {Id : int, Bar : string} PRIMARY KEY Id
table foo10 : {Id : int, Bar : string} PRIMARY KEY Id
table tab : {Id : int, Val : int} PRIMARY KEY Id

fun cache01 () =
    res <- oneOrNoRows (SELECT foo01.Bar FROM foo01 WHERE foo01.Id = 42);
    return <xml><body>
      Reading 1.
      {case res of
           None => <xml>?</xml>
         | Some row => <xml>{[row.Foo01.Bar]}</xml>}
    </body></xml>

fun cache10 () =
    res <- queryX (SELECT foo10.Bar FROM foo10 WHERE foo10.Id = 42)
                  (fn row => <xml>{[row.Foo10.Bar]}</xml>);
    return <xml><body>
      Reading 2.
      {res}
    </body></xml>

fun cache11 () =
    res <- oneOrNoRows (SELECT foo01.Bar FROM foo01 WHERE foo01.Id = 42);
    bla <- oneOrNoRows (SELECT foo10.Bar FROM foo10 WHERE foo10.Id = 42);
    return <xml><body>
      Reading 1 and 2.
      {case res of
           None => <xml>?</xml>
         | Some row => <xml>{[row.Foo01.Bar]}</xml>}
      {case bla of
           None => <xml>?</xml>
         | Some row => <xml>{[row.Foo10.Bar]}</xml>}
    </body></xml>

fun flush01 () =
    dml (UPDATE foo01 SET Bar = "baz01" WHERE Id = 42);
    return <xml><body>
      Flushed 1!
    </body></xml>

fun flush10 () =
    dml (UPDATE foo10 SET Bar = "baz10" WHERE Id = 42);
    return <xml><body>
      Flushed 2!
    </body></xml>

fun flush11 () =
    dml (UPDATE foo01 SET Bar = "baz11" WHERE Id = 42);
    dml (UPDATE foo10 SET Bar = "baz11" WHERE Id = 42);
    return <xml><body>
      Flushed 1 and 2!
    </body></xml>

fun cache id =
    res <- oneOrNoRows (SELECT tab.Val FROM tab WHERE tab.Id = {[id]});
    return <xml><body>
      Reading {[id]}.
      {case res of
           None => <xml>?</xml>
         | Some row => <xml>{[row.Tab.Val]}</xml>}
    </body></xml>

fun flush id =
    res <- oneOrNoRows (SELECT tab.Val FROM tab WHERE tab.Id = {[id]});
    dml (case res of
             None => (INSERT INTO tab (Id, Val) VALUES ({[id]}, 0))
           | Some row => (UPDATE tab SET Val = {[row.Tab.Val + 1]} WHERE Id = {[id]}));
    return <xml><body>
      (* Flushed {[id]}! *)
      {case res of
           None => <xml>Initialized {[id]}!</xml>
         | Some row => <xml>Incremented {[id]}!</xml>}
    </body></xml>
