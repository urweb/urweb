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

con aggregateMeta = fn (row :: {Type}) (acc :: Type) =>
                       {Initial : acc,
                        Step : $row -> acc -> acc,
                        Display : acc -> xbody}

structure Direct = struct
    con metaBase = fn actual_input :: (Type * Type) =>
                  {Display : actual_input.2 -> xbody,
                   Edit : actual_input.2 -> xbody,
                   Initialize : actual_input.1 -> transaction actual_input.2,
                   Parse : actual_input.2 -> signal (option actual_input.1)}

    datatype metaBoth actual input =
             NonNull of metaBase (actual, input) * metaBase (option actual, input)
           | Nullable of metaBase (actual, input)

    con meta = fn global_actual_input :: (Type * Type * Type) =>
                  {Initialize : transaction global_actual_input.1,
                   Handlers : global_actual_input.1
                              -> metaBoth global_actual_input.2 global_actual_input.3}

    con editableState (ts :: (Type * Type * Type)) = (ts.1, ts.3)
    fun editable [ts] [rest] [nm :: Name] [[nm] ~ rest] name (m : meta ts) : colMeta ([nm = ts.2] ++ rest)
                                                                                     (editableState ts) =
       let
           fun doMr mr = {Header = name,
                          Project = fn r => mr.Initialize r.nm,
                          Update = fn r s =>
                                      vo <- current (mr.Parse s);
                                      return (case vo of
                                                  None => r
                                                | Some v => r -- nm ++ {nm = v}),
                          Display = mr.Display,
                          Edit = mr.Edit,
                          Validate = fn s => vo <- mr.Parse s; return (Option.isSome vo)}
       in
           {Initialize = m.Initialize,
            Handlers = fn data => case m.Handlers data of
                                  NonNull (mr, _) => doMr mr
                                | Nullable mr => doMr mr}
       end
        
    con readOnlyState (ts :: (Type * Type * Type)) = (ts.1, ts.3)
    fun readOnly [ts] [rest] [nm :: Name] [[nm] ~ rest] name (m : meta ts) : colMeta ([nm = ts.2] ++ rest)
                                                                                     (readOnlyState ts) =
       let
           fun doMr mr = {Header = name,
                          Project = fn r => mr.Initialize r.nm,
                          Update = fn r _ => return r,
                          Display = mr.Display,
                          Edit = mr.Display,
                          Validate = fn _ => return True}
       in
           {Initialize = m.Initialize,
            Handlers = fn data => case m.Handlers data of
                                      NonNull (mr, _) => doMr mr
                                    | Nullable mr => doMr mr}
       end

    con metaBasic = fn actual_input :: (Type * Type) =>
                       {Display : actual_input.2 -> xbody,
                        Edit : source actual_input.2 -> xbody,
                        Initialize : actual_input.1 -> actual_input.2,
                        InitializeNull : actual_input.2,
                        IsNull : actual_input.2 -> bool,
                        Parse : actual_input.2 -> option actual_input.1}

    con basicState = source
    fun basic [ts ::: (Type * Type)] (m : metaBasic ts) : meta (unit, ts.1, basicState ts.2) =
        {Initialize = return (),
         Handlers = fn () => NonNull (
                             {Display = fn s => <xml><dyn signal={v <- signal s; return (m.Display v)}/></xml>,
                              Edit = m.Edit,
                              Initialize = fn v => source (m.Initialize v),
                              Parse = fn s => v <- signal s; return (m.Parse v)},
                             {Display = fn s => <xml><dyn signal={v <- signal s; return (m.Display v)}/></xml>,
                              Edit = m.Edit,
                              Initialize = fn v => source (case v of
                                                       None => m.InitializeNull
                                                     | Some v => m.Initialize v),
                              Parse = fn s => v <- signal s;
                                         return (if m.IsNull v then
                                                     Some None
                                                 else
                                                     case m.Parse v of
                                                         None => None
                                                       | Some v' => Some (Some v'))})}

    fun nullable [global] [actual] [input] (m : meta (global, actual, input)) =
        {Initialize = m.Initialize,
         Handlers = fn d => case m.Handlers d of
                                Nullable _ => error <xml>Don't stack calls to Direct.nullable!</xml>
                              | NonNull (_, ho) => Nullable ho}

    type intGlobal = unit
    type intInput = basicState string
    val int : meta (intGlobal, int, intInput) =
        basic {Display = fn s => <xml>{[s]}</xml>,
               Edit = fn s => <xml><ctextbox source={s}/></xml>,
               Initialize = fn n => show n,
               InitializeNull = "",
               IsNull = eq "",
               Parse = fn v => read v}

    type stringGlobal = unit
    type stringInput = basicState string
    val string : meta (stringGlobal, string, stringInput) =
        basic {Display = fn s => <xml>{[s]}</xml>,
               Edit = fn s => <xml><ctextbox source={s}/></xml>,
               Initialize = fn s => s,
               InitializeNull = "",
               IsNull = eq "",
               Parse = fn s => Some s}

    type boolGlobal = unit
    type boolInput = basicState bool
    val bool : meta (boolGlobal, bool, boolInput) =
        basic {Display = fn b => <xml>{[b]}</xml>,
               Edit = fn s => <xml><ccheckbox source={s}/></xml>,
               Initialize = fn b => b,
               InitializeNull = False,
               IsNull = fn _ => False,
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
        con input = source string * option (t * $row)

        val getChoices = List.mapQuery (SELECT * FROM tab AS T)
                                       (fn r => (r.T.nm, render r.T))

        fun getChoice k =
            r <- oneRow (SELECT T.{{row}} FROM tab AS T WHERE T.{nm} = {[k]});
            return r.T

        val meta : meta (global, M.t, input) =
            {Initialize = getChoices,
             Handlers = fn choices =>
                           NonNull (
                           {Display = fn (_, kr) => case kr of
                                                          None => error <xml>Unexpected Foreign null</xml>
                                                        | Some (k, r) => <xml>{[render ({nm = k} ++ r)]}</xml>,
                            Edit = fn (s, kr) => 
                                      <xml><cselect source={s}>
                                        {List.mapX (fn (k', rend) =>
                                                       <xml><coption value={show k'} selected={case kr of
                                                                                                   None => False
                                                                                                 | Some (k, _) =>
                                                                                                   k' = k}>{[rend]}</coption>
                                                       </xml>)
                                                   choices}
                                      </cselect></xml>,
                            Initialize = fn k => s <- source (show k);
                                            r <- rpc (getChoice k);
                                            return (s, Some (k, r)),
                            Parse = fn (s, _) => k <- signal s; return (read k : option t)},
                           {Display = fn (_, kr) => case kr of
                                                          None => <xml>NULL</xml>
                                                        | Some (k, r) => <xml>{[render ({nm = k} ++ r)]}</xml>,
                            Edit = fn (s, kr) => 
                                      <xml><cselect source={s}>
                                        <coption value="" selected={case kr of
                                                                        None => True
                                                                      | _ => False}>NULL</coption>
                                        {List.mapX (fn (k', rend) =>
                                                       <xml><coption value={show k'} selected={case kr of
                                                                                                   None => False
                                                                                                 | Some (k, _) =>
                                                                                                   k' = k}>{[rend]}</coption>
                                                       </xml>)
                                                   choices}
                                      </cselect></xml>,
                            Initialize = fn k => case k of
                                                     None =>
                                                     s <- source "";
                                                     return (s, None)
                                                   | Some k =>
                                                     s <- source (show k);
                                                     r <- rpc (getChoice k);
                                                     return (s, Some (k, r)),
                            Parse = fn (s, _) => ks <- signal s;
                                       return (case ks of
                                                   "" => Some None
                                                 | _ => case read ks : option t of
                                                            None => None
                                                          | Some k => Some (Some k))})}
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

                 con aggregates :: {Type}
                 val aggregates : $(map (aggregateMeta (key ++ row)) aggregates)
             end) = struct
    open Grid.Make(struct
                       fun keyOf r = r --- M.row

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

                       fun selector (r : $M.key) : sql_exp [T = M.key ++ M.row] [] [] bool =
                         foldR2 [rawMeta] [id]
                                [fn key => rest :: {Type} -> [rest ~ key] => sql_exp [T = key ++ rest] [] [] bool]
                                (fn [nm :: Name] [t :: Type] [key :: {Type}] [[nm] ~ key]
                                    (meta : rawMeta t) (v : t)
                                    (exp : rest :: {Type} -> [rest ~ key] => sql_exp [T = key ++ rest] [] [] bool)
                                    [rest :: {Type}] [rest ~ [nm = t] ++ key] =>
                                    (WHERE T.{nm} = {@sql_inject meta.Inj v} AND {exp [[nm = t] ++ rest] !}))
                                (fn [rest :: {Type}] [rest ~ []] => (WHERE TRUE))
                                [_] M.keyFolder (M.raw --- map rawMeta M.row) r
                                [_] !

                       fun save key row =
                           dml (update [M.key ++ M.row] !
                                       (ensql row)
                                       M.tab
                                       (selector key))

                       fun delete key =
                           dml (Basis.delete M.tab (selector key))

                       val cols = M.cols

                       val folder = M.colsFolder

                       val aggregates = M.aggregates
                   end)
end
