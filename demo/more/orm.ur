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

    con fs = [Id = id] ++ M.cols

    sequence s
    table t : fs

    type row = $fs

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

    fun resultsOut q = query q (fn r ls => return (r.T :: ls)) []

    val list = resultsOut (SELECT * FROM t)

    con col = fn t => {Exp : sql_exp [T = fs] [] [] t,
                       Inj : sql_injectable t}
    val idCol = {Exp = sql_field [#T] [#Id], Inj = _}
    val cols = foldR [meta] [fn before => after :: {Type} -> [before ~ after] =>
                                $(map (fn t => {Exp : sql_exp [T = before ++ after] [] [] t,
                                                Inj : sql_injectable t}) before)]
               (fn [nm :: Name] [t :: Type] [before :: {Type}] [[nm] ~ before] (meta : meta t)
                                (acc : after :: {Type} -> [before ~ after] =>
                                 $(map (fn t => {Exp : sql_exp [T = before ++ after] [] [] t,
                                                 Inj : sql_injectable t}) before))
                                [after :: {Type}] [[nm = t] ++ before ~ after] =>
                   {nm = {Exp = sql_field [#T] [nm],
                          Inj = meta.Inj}} ++ acc [[nm = t] ++ after] !)
               (fn [after :: {Type}] [[] ~ after] => {})
               [_] M.folder M.cols
               [[Id = id]] !

    type filter = sql_exp [T = fs] [] [] bool
    fun search (f : filter) = resultsOut (SELECT * FROM t WHERE {f})

    fun bin (b : t ::: Type -> sql_binary t t bool) [t] (c : col t) (v : t) =
        sql_binary b c.Exp (@sql_inject c.Inj v)
    val eq = bin @@sql_eq
    val ne = bin @@sql_ne
    val lt = bin @@sql_lt
    val le = bin @@sql_le
    val gt = bin @@sql_gt
    val ge = bin @@sql_ge

    fun bb (b : sql_binary bool bool bool) (f1 : filter) (f2 : filter) =
        sql_binary b f1 f2
    val _and = bb sql_and
    val or = bb sql_or
end
