table foo01 : {Id : int, Bar : string} PRIMARY KEY Id
table foo10 : {Id : int, Bar : string} PRIMARY KEY Id

(* val query = (SELECT * FROM foo WHERE foo.Bar = "baz") *)
(* val insert = (INSERT INTO foo (Id, Bar) VALUES (42, "baz")) *)

fun flush01 () : transaction page=
    dml (INSERT INTO foo01 (Id, Bar) VALUES (42, "baz"));
    return
        <xml>
          <body>
            Flushed 1!
          </body>
        </xml>

fun flush10 () : transaction page=
    dml (INSERT INTO foo10 (Id, Bar) VALUES (42, "baz"));
    return
        <xml>
          <body>
            Flushed 2!
          </body>
        </xml>

fun flush11 () : transaction page=
    dml (INSERT INTO foo01 (Id, Bar) VALUES (42, "baz"));
    dml (INSERT INTO foo10 (Id, Bar) VALUES (42, "baz"));
    return
        <xml>
          <body>
            Flushed 1 and 2!
          </body>
        </xml>

fun cache01 () : transaction page =
    res <- oneOrNoRows (SELECT foo01.Id, foo01.Bar
                        FROM foo01
                        WHERE foo01.Bar = "baz");
    return
        <xml>
          <body>
            Reading 1.
            {case res of
                 None => <xml></xml>
               | Some row => <xml>{[row.Foo01.Bar]}</xml>}
          </body>
        </xml>

fun cache10 () : transaction page =
    res <- oneOrNoRows (SELECT foo10.Id, foo10.Bar
                        FROM foo10
                        WHERE foo10.Bar = "baz");
    return
        <xml>
          <body>
            Reading 2.
            {case res of
                 None => <xml></xml>
               | Some row => <xml>{[row.Foo10.Bar]}</xml>}
          </body>
        </xml>

fun cache11 () : transaction page =
    res <- oneOrNoRows (SELECT foo01.Id, foo01.Bar
                        FROM foo01
                        WHERE foo01.Bar = "baz");
    bla <- oneOrNoRows (SELECT foo10.Id, foo10.Bar
                        FROM foo10
                        WHERE foo10.Bar = "baz");
    return
        <xml>
          <body>
            Reading 1 and 2.
            {case res of
                 None => <xml></xml>
               | Some row => <xml>{[row.Foo01.Bar]}</xml>}
            {case bla of
                 None => <xml></xml>
               | Some row => <xml>{[row.Foo10.Bar]}</xml>}
          </body>
        </xml>
