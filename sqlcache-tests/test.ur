table foo : {Id : int, Bar : string} PRIMARY KEY Id

(* val query = (SELECT * FROM foo WHERE foo.Bar = "baz") *)
(* val insert = (INSERT INTO foo (Id, Bar) VALUES (42, "baz")) *)

fun main () : transaction page =
    dml (INSERT INTO foo (Id, Bar) VALUES (42, "baz"));
    res <- oneOrNoRows (SELECT foo.Id, foo.Bar
                        FROM foo
                        WHERE foo.Bar = "baz"
                          UNION
                        SELECT *
                        FROM foo
                        WHERE foo.Bar = "qux");
    return
        <xml>
          <body>
            {case res of
                 None => <xml></xml>
               | Some row => <xml>{[row.Foo.Bar]}</xml>}
          </body>
        </xml>
