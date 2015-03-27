table tab : {Id : int, Val : int} PRIMARY KEY Id

fun cache id =
    res <- oneOrNoRows (SELECT tab.Val
                        FROM tab
                        WHERE tab.Id = {[id]});
    return <xml><body>
      Reading {[id]}.
      {case res of
           None => <xml>?</xml>
         | Some row => <xml>{[row.Tab.Val]}</xml>}
    </body></xml>

fun flush id =
    res <- oneOrNoRows (SELECT tab.Val
                        FROM tab
                        WHERE tab.Id = {[id]});
    (case res of
         None => dml (INSERT INTO tab (Id, Val)
                      VALUES ({[id]}, 0))
       | Some row => dml (UPDATE tab
                          SET Val = {[row.Tab.Val + 1]}
                          WHERE Id = {[id]}));
    return <xml><body>
      {case res of
           None => <xml>Initialized {[id]}!</xml>
         | Some row => <xml>Incremented {[id]}!</xml>}
    </body></xml>
