open Orm

structure T = Table(struct
                        val cols = {A = local [int],
                                    B = local [string]}
                    end)

structure S = Table(struct
                        val cols = {C = T.id,
                                    D = local [float]}
                    end)

fun action () =
    r1 <- T.create {A = 3, B = "Hi"};
    T.save (r1 -- #B ++ {B = "Bye"});
    r2 <- T.create {A = 4, B = "Why"};
    r3 <- T.create {A = 66, B = "Hi"};

    s <- S.create {C = r1.Id, D = 45.67};

    ls <- T.list;
    ls' <- T.search (T.eq T.cols.B.Col "Hi");

    lsS <- S.list;
    lsS <- List.mapM (fn r => p <- S.cols.C.Parent r; return (r, p)) lsS;

    T.delete r1;
    T.delete r2;
    T.delete r3;

    S.delete s;

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
