table tab : {Id : int, Val : int} PRIMARY KEY Id

fun check id =
    res <- oneOrNoRows (SELECT tab.Val FROM tab WHERE tab.Id = {[id]});
    return <xml><body>
      {case res of
           None => <xml>?</xml>
         | Some row => <xml>{[row.Tab.Val]}</xml>}
    </body></xml>

fun flush id =
    dml (UPDATE tab SET Val = Val + 1 WHERE Id = {[id]});
    return <xml><body>
      Changed {[id]}!
    </body></xml>

fun main x y =
    r <- rand;
    let
        val id = r % x
        val doFlush = (r / x) % y = 0
    in
        if doFlush then flush id else check id
    end
