con rawMeta = fn t :: Type =>
                 {New : transaction t,
                  Inj : sql_injectable t}

con colMeta' = fn (row :: {Type}) (t :: Type) =>
                  {Header : string,
                   Project : $row -> transaction t,
                   Update : $row -> t -> transaction ($row),
                   Display : t -> xbody,
                   Edit : t -> xbody,
                   Validate : t -> signal bool}

con colMeta = fn (row :: {Type}) (global_t :: (Type * Type)) =>
                 {Initialize : transaction global_t.1,
                  Handlers : global_t.1 -> colMeta' row global_t.2}

structure Direct = struct
    con meta = fn global_actual_input :: (Type * Type * Type) =>
                  {Initialize : transaction global_actual_input.1,
                   Handlers : global_actual_input.1
                              -> {Display : global_actual_input.3 -> xbody,
                                  Edit : global_actual_input.3 -> xbody,
                                  Initialize : global_actual_input.2 -> transaction global_actual_input.3,
                                  Parse : global_actual_input.3 -> signal (option global_actual_input.2)}}

    con editableState (ts :: (Type * Type * Type)) = (ts.1, ts.3)
    fun editable [ts] [rest] [nm :: Name] [[nm] ~ rest] name (m : meta ts) : colMeta ([nm = ts.2] ++ rest)
                                                                                     (editableState ts) =
       {Initialize = m.Initialize,
        Handlers = fn data => {Header = name,
                               Project = fn r => (m.Handlers data).Initialize r.nm,
                               Update = fn r s =>
                                           vo <- current ((m.Handlers data).Parse s);
                                           return (case vo of
                                                       None => r
                                                     | Some v => r -- nm ++ {nm = v}),
                               Display = (m.Handlers data).Display,
                               Edit = (m.Handlers data).Edit,
                               Validate = fn s => vo <- (m.Handlers data).Parse s; return (Option.isSome vo)}}
        
    con readOnlyState (ts :: (Type * Type * Type)) = (ts.1, ts.3)
    fun readOnly [ts] [rest] [nm :: Name] [[nm] ~ rest] name (m : meta ts) : colMeta ([nm = ts.2] ++ rest)
                                                                                     (readOnlyState ts) =
       {Initialize = m.Initialize,
        Handlers = fn data => {Header = name,
                               Project = fn r => (m.Handlers data).Initialize r.nm,
                               Update = fn r _ => return r,
                               Display = (m.Handlers data).Display,
                               Edit = (m.Handlers data).Display,
                               Validate = fn _ => return True}}

    con metaBasic = fn actual_input :: (Type * Type) =>
                  {Display : actual_input.2 -> xbody,
                   Edit : source actual_input.2 -> xbody,
                   Initialize : actual_input.1 -> actual_input.2,
                   Parse : actual_input.2 -> option actual_input.1}

    con basicState = source
    fun basic [ts ::: (Type * Type)] (m : metaBasic ts) : meta (unit, ts.1, basicState ts.2) =
        {Initialize = return (),
         Handlers = fn () => {Display = fn s => <xml><dyn signal={v <- signal s; return (m.Display v)}/></xml>,
                              Edit = m.Edit,
                              Initialize = fn v => source (m.Initialize v),
                              Parse = fn s => v <- signal s; return (m.Parse v)}}

    type intGlobal = unit
    type intInput = basicState string
    val int : meta (intGlobal, int, intInput) =
        basic {Display = fn s => <xml>{[s]}</xml>,
               Edit = fn s => <xml><ctextbox source={s}/></xml>,
               Initialize = fn n => show n,
               Parse = fn v => read v}

    type stringGlobal = unit
    type stringInput = basicState string
    val string : meta (stringGlobal, string, stringInput) =
        basic {Display = fn s => <xml>{[s]}</xml>,
               Edit = fn s => <xml><ctextbox source={s}/></xml>,
               Initialize = fn s => s,
               Parse = fn s => Some s}

    type boolGlobal = unit
    type boolInput = basicState bool
    val bool : meta (boolGlobal, bool, boolInput) =
        basic {Display = fn b => <xml>{[b]}</xml>,
               Edit = fn s => <xml><ccheckbox source={s}/></xml>,
               Initialize = fn b => b,
               Parse = fn b => Some b}

    functor Foreign (M : sig
                         con row :: {Type}
                         con t :: Type
                         val show_t : show t
                         val read_t : read t
                         val eq_t : eq t
                         val inj_t : sql_injectable t
                         con nm :: Name
                         constraint [nm] ~ row
                         table tab : ([nm = t] ++ row)
                         val render : $([nm = t] ++ row) -> string
                     end) = struct
        open M

        con global = list (t * string)
        con input = source string * t * $row

        val getChoices = List.mapQuery (SELECT * FROM tab AS T)
                                       (fn r => (r.T.nm, render r.T))

        fun getChoice k =
            r <- oneRow (SELECT T.{{row}} FROM tab AS T WHERE T.{nm} = {[k]});
            return r.T

        val meta =
            {Initialize = getChoices,
             Handlers = fn choices =>
                           {Display = fn (_, k, r) => <xml>{[render ({nm = k} ++ r)]}</xml>,
                            Edit = fn (s, k, _) => 
                                      <xml><cselect source={s}>
                                        {List.mapX (fn (k', rend) =>
                                                       <xml><coption value={show k'} selected={k' = k}>{[rend]}</coption>
                                                       </xml>)
                                                   choices}
                                      </cselect></xml>,
                            Initialize = fn k => s <- source (show k);
                                            r <- rpc (getChoice k);
                                            return (s, k, r),
                            Parse = fn (s, _, _) => k <- signal s; return (read k)}}
    end
end

con computedState = (unit, xbody)
fun computed [row] [t] (_ : show t) name (f : $row -> t) : colMeta row computedState =
    {Initialize = return (),
     Handlers = fn () => {Header = name,
                          Project = fn r => return <xml>{[f r]}</xml>,
                          Update = fn r _ => return r,
                          Display = fn x => x,
                          Edit = fn _ => <xml>...</xml>,
                          Validate = fn _ => return True}}
fun computedHtml [row] name (f : $row -> xbody) : colMeta row computedState =
    {Initialize = return (),
     Handlers = fn () => {Header = name,
                          Project = fn r => return (f r),
                          Update = fn r _ => return r,
                          Display = fn x => x,
                          Edit = fn _ => <xml>...</xml>,
                          Validate = fn _ => return True}}

functor Make(M : sig
                 con key :: {Type}
                 con row :: {Type}
                 constraint key ~ row
                 table tab : (key ++ row)

                 val raw : $(map rawMeta (key ++ row))

                 con cols :: {(Type * Type)}
                 val cols : $(map (colMeta (key ++ row)) cols)

                 val keyFolder : folder key
                 val rowFolder : folder row
                 val colsFolder : folder cols
             end) = struct
    open Grid.Make(struct
                       val list = query (SELECT * FROM {{M.tab}} AS T) (fn r rs => return (r.T :: rs)) []

                       val wholeRow = @Folder.concat ! M.keyFolder M.rowFolder

                       fun ensql [env] (r : $(M.key ++ M.row)) =
                           map2 [rawMeta] [id] [sql_exp env [] []]
                           (fn [t] meta v => @sql_inject meta.Inj v)
                           [_] wholeRow M.raw r

                       val new =
                           row <- Monad.mapR [rawMeta] [id]
                                              (fn [nm :: Name] [t :: Type] meta => meta.New)
                                              [_] wholeRow M.raw;
                           dml (insert M.tab (ensql row));
                           return row

                       fun selector (r : $(M.key ++ M.row)) : sql_exp [T = M.key ++ M.row] [] [] bool =
                         foldR2 [rawMeta] [id]
                                [fn key => rest :: {Type} -> [rest ~ key] => sql_exp [T = key ++ rest] [] [] bool]
                                (fn [nm :: Name] [t :: Type] [key :: {Type}] [[nm] ~ key]
                                    (meta : rawMeta t) (v : t)
                                    (exp : rest :: {Type} -> [rest ~ key] => sql_exp [T = key ++ rest] [] [] bool)
                                    [rest :: {Type}] [rest ~ [nm = t] ++ key] =>
                                    (WHERE T.{nm} = {@sql_inject meta.Inj v} AND {exp [[nm = t] ++ rest] !}))
                                (fn [rest :: {Type}] [rest ~ []] => (WHERE TRUE))
                                [_] M.keyFolder (M.raw --- map rawMeta M.row) (r --- M.row)
                                [_] !

                       fun save {Old = row, New = row'} =
                           dml (update [M.key ++ M.row] !
                                       (ensql row')
                                       M.tab
                                       (selector row))

                       fun delete row =
                           dml (Basis.delete M.tab (selector row))

                       val cols = M.cols

                       val folder = M.colsFolder
                   end)
end
