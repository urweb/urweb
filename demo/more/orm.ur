con link = fn col_parent :: (Type * Type) => col_parent.1 -> transaction (option col_parent.2)
fun noParent [t ::: Type] (_ : t) : transaction (option unit) = return None

con meta = fn (col :: Type, parent :: Type) => {
	      Link : link (col, parent),
	      Inj : sql_injectable col
	      }

fun local [t :: Type] (inj : sql_injectable t) : meta (t, unit) =
    {Link = noParent,
     Inj = inj}

functor Table(M : sig
                  con cols :: {(Type * Type)}
                  val cols : $(map meta cols)
                  constraint [Id] ~ cols
                  val folder : folder cols
              end) = struct
    type id = int
    con fs' = map fst M.cols
    con fs = [Id = id] ++ fs'
    type row' = $fs'
    type row = $fs

    fun resultsOut q = query q (fn r ls => return (r.T :: ls)) []
    fun resultOut q = ro <- oneOrNoRows q; return (Option.mp (fn r => r .T) ro)

    sequence s
    table t : fs

    val inj = _
    val id = {Link = fn id => resultOut (SELECT * FROM t WHERE t.Id = {[id]}),
              Inj = inj}

    fun ensql [avail ::_] (r : row') : $(map (sql_exp avail [] []) fs') =
        @map2 [meta] [fst] [fn ts :: (Type * Type) => sql_exp avail [] [] ts.1]
         (fn [ts] meta v => @sql_inject meta.Inj v)
         M.folder M.cols r

    fun create (r : row') =
        id <- nextval s;
        dml (insert t ({Id = sql_inject id} ++ ensql [[]] r));
        return ({Id = id} ++ r)

    fun delete r = dml (DELETE FROM t WHERE t.Id = {[r.Id]})

    fun save r = dml (update [fs'] (ensql [[T = [Id = int] ++ map fst M.cols]] (r -- #Id)) t (WHERE T.Id = {[r.Id]}))

    fun lookup id =
        ro <- oneOrNoRows (SELECT * FROM t WHERE t.Id = {[id]});
        return (Option.mp (fn r => r.T) ro)


    val list = resultsOut (SELECT * FROM t)

    con col = fn t => {Exp : sql_exp [T = fs] [] [] t,
                       Inj : sql_injectable t}
    val idCol = {Exp = sql_field [#T] [#Id], Inj = _}
    con meta' = fn (fs :: {Type}) (col :: Type, parent :: Type) =>
                   {Col : {Exp : sql_exp [T = fs] [] [] col,
                           Inj : sql_injectable col},
                    Parent : $fs -> transaction (option parent)}
    val cols = @foldR [meta] [fn before => after :: {(Type * Type)} -> [before ~ after] =>
                                 $(map (meta' (map fst (before ++ after))) before)]
                (fn [nm :: Name] [ts :: (Type * Type)] [before :: {(Type * Type)}]
                                 [[nm] ~ before] (meta : meta ts)
                                 (acc : after :: {(Type * Type)} -> [before ~ after] =>
                                  $(map (meta' (map fst (before ++ after))) before))
                                 [after :: {(Type * Type)}] [[nm = ts] ++ before ~ after] =>
                    {nm = {Col = {Exp = sql_field [#T] [nm],
                                  Inj = meta.Inj},
                           Parent = fn r => meta.Link r.nm}}
                        ++ acc [[nm = ts] ++ after] !)
                (fn [after :: {(Type * Type)}] [[] ~ after] => {})
                M.folder M.cols
                [[Id = (id, row)]] !

    type filter = sql_exp [T = fs] [] [] bool
    fun find (f : filter) = resultOut (SELECT * FROM t WHERE {f})
    fun search (f : filter) = resultsOut (SELECT * FROM t WHERE {f})

    fun bin (b : t ::: Type -> sql_binary t t bool) [t] (c : col t) (v : t) =
        sql_binary b c.Exp (@sql_inject c.Inj v)
    val eq = @@bin @@sql_eq
    val ne = @@bin @@sql_ne
    val lt = @@bin @@sql_lt
    val le = @@bin @@sql_le
    val gt = @@bin @@sql_gt
    val ge = @@bin @@sql_ge

    fun bb (b : sql_binary bool bool bool) (f1 : filter) (f2 : filter) =
        sql_binary b f1 f2
    val _and = bb sql_and
    val or = bb sql_or
end
