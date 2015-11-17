table tab : {Id : int, Val : int} PRIMARY KEY Id

fun check id =
    res <- oneOrNoRows (SELECT tab.Val FROM tab WHERE tab.Id = {[id]});
    return <xml><body>
      cache
      {case res of
           None => <xml>?</xml>
         | Some row => <xml>{[row.Tab.Val]}</xml>}
    </body></xml>

fun flush id =
    dml (UPDATE tab SET Val = Val + 1 WHERE Id = {[id]});
    return <xml><body>
      Changed {[id]}!
    </body></xml>
