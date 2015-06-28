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
         None => return () (* dml (INSERT INTO tab (Id, Val) *)
                 (*      VALUES ({[id]}, 0)) *)
       | Some row => dml (UPDATE tab
                          SET Val = {[row.Tab.Val + 1]}
                          WHERE Id = {[id + 1]} OR Id = {[id]} (* OR Id = {[id - 1]} *)));
    return <xml><body>
      {case res of
           None => <xml>Initialized {[id]}!</xml>
         | Some row => <xml>Incremented {[id]}!</xml>}
    </body></xml>

(* task periodic 5 = *)
(*  fn () => *)
(*     t <- now; *)
(*     let *)
(*         val n = toSeconds t % 2 *)
(*     in *)
(*         dml (UPDATE tab *)
(*              SET Val = 9001 *)
(*              WHERE Id = {[n]} OR Id = {[n+1]}) *)
(*     end *)
