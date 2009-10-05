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

    fun create (r : $M.cols) =
        id <- nextval s;
        dml (insert t ({Id = sql_inject id}
                           ++ map2 [meta] [Top.id] [sql_exp [] [] []]
                           (fn [t ::: Type] (meta : meta t) (v : t) => @sql_inject meta.Inj v)
                           [_] M.folder M.cols r));
        return id
end
