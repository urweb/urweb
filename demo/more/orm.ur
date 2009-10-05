con link = fn t :: Type => unit

con meta = fn col :: Type => {
              Link : link col,
              Inj : sql_injectable col
              }

functor Table(M : sig
                  con cols :: {Type}
                  val cols : $(map meta cols)
                  constraint [Id] ~ cols
                  val folder : folder cols
              end) = struct
    type id = int
    val inj = _
    val id : meta id = {Link = (),
                        Inj = inj}

    sequence s
    table t : ([Id = id] ++ M.cols)

    type row = $([Id = id] ++ M.cols)

    fun ensql [avail] (r : $M.cols) : $(map (sql_exp avail [] []) M.cols) =
        map2 [meta] [Top.id] [sql_exp avail [] []]
             (fn [t] meta v => @sql_inject meta.Inj v)
             [_] M.folder M.cols r

    fun create (r : $M.cols) =
        id <- nextval s;
        dml (insert t ({Id = sql_inject id} ++ ensql r));
        return ({Id = id} ++ r)

    fun delete r = dml (DELETE FROM t WHERE t.Id = {[r.Id]})

    fun save r = dml (update [M.cols] ! (ensql (r -- #Id)) t (WHERE T.Id = {[r.Id]}))

    fun lookup id =
        ro <- oneOrNoRows (SELECT * FROM t WHERE t.Id = {[id]});
        return (Option.mp (fn r => r.T) ro)

    val list = query (SELECT * FROM t) (fn r ls => return (r.T :: ls)) []
end
