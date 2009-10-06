open Orm

structure T = Table(struct
                        val cols = {A = local [int] _,
                                    B = local [string] _}
                    end)

structure S = Table(struct
                        val cols = {C = T.id,
                                    D = local [float] _}
                    end)

fun action () =
    r <- T.create {A = 3, B = "Hi"};
    T.save (r -- #B ++ {B = "Bye"});

    s <- S.create {C = r.Id, D = 45.67};

    ls <- T.list;
    ls' <- T.search (T.eq T.cols.B.Col "Hi");

    lsS <- S.list;
    lsS <- List.mapM (fn r => p <- S.cols.C.Parent r; return (r, p)) lsS;

    return <xml><body>
      {List.mapX (fn r => <xml><li> {[r.A]}: {[r.B]}</li></xml>) ls}
      <br/>
      {List.mapX (fn r => <xml><li> {[r.A]}: {[r.B]}</li></xml>) ls'}
      <br/>
      {List.mapX (fn (s, ro) => <xml><li> {[s.D]}: {case ro of
                                                        None => <xml>No parent</xml>
                                                      | Some r => <xml>{[r.B]}</xml>}
      </li></xml>) lsS}
    </body></xml>
    
fun main () = return <xml><body>
  <form><submit action={action}/></form>
</body></xml>
