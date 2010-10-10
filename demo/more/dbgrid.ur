con rawMeta = fn t :: Type =>
                 {New : transaction t,
                  Inj : sql_injectable t}

con colMeta' = fn (row :: {Type}) (input :: Type) (filter :: Type) =>
                  {Header : string,
                   Project : $row -> transaction input,
                   Update : $row -> input -> transaction ($row),
                   Display : input -> xbody,
                   Edit : input -> xbody,
                   Validate : input -> signal bool,
                   CreateFilter : transaction filter,
                   DisplayFilter : filter -> xbody,
                   Filter : filter -> $row -> signal bool,
                   Sort : option ($row -> $row -> bool)}

con colMeta = fn (row :: {Type}) (global_input_filter :: (Type * Type * Type)) =>
                 {Initialize : transaction global_input_filter.1,
                  Handlers : global_input_filter.1 -> colMeta' row global_input_filter.2 global_input_filter.3}

con aggregateMeta = fn (row :: {Type}) (acc :: Type) =>
                       {Initial : acc,
                        Step : $row -> acc -> acc,
                        Display : acc -> xbody}

structure Direct = struct
    con metaBase = fn actual_input_filter :: (Type * Type * Type) =>
                  {Display : actual_input_filter.2 -> xbody,
                   Edit : actual_input_filter.2 -> xbody,
                   Initialize : actual_input_filter.1 -> transaction actual_input_filter.2,
                   Parse : actual_input_filter.2 -> signal (option actual_input_filter.1),
                   CreateFilter : transaction actual_input_filter.3,
                   DisplayFilter : actual_input_filter.3 -> xbody,
                   Filter : actual_input_filter.3 -> actual_input_filter.1 -> signal bool,
                   Sort : actual_input_filter.1 -> actual_input_filter.1 -> bool}

    datatype metaBoth actual input filter =
             NonNull of metaBase (actual, input, filter) * metaBase (option actual, input, filter)
           | Nullable of metaBase (actual, input, filter)

    con meta = fn global_actual_input_filter =>
                  {Initialize : transaction global_actual_input_filter.1,
                   Handlers : global_actual_input_filter.1
                              -> metaBoth global_actual_input_filter.2 global_actual_input_filter.3
                                          global_actual_input_filter.4}

    con editableState (ts :: (Type * Type * Type * Type)) = (ts.1, ts.3, ts.4)
    fun editable [ts] [rest] [nm :: Name] [[nm] ~ rest] name (m : meta ts) : colMeta ([nm = ts.2] ++ rest)
                                                                                     (editableState ts) =
       let
           fun doMr (mr : metaBase (ts.2, ts.3, ts.4)) : colMeta' ([nm = ts.2] ++ rest) ts.3 ts.4 =
               {Header = name,
                Project = fn r => mr.Initialize r.nm,
                Update = fn r s =>
                            vo <- current (mr.Parse s);
                            return (case vo of
                                        None => r
                                      | Some v => r -- nm ++ {nm = v}),
                Display = mr.Display,
                Edit = mr.Edit,
                Validate = fn s => vo <- mr.Parse s; return (Option.isSome vo),
                CreateFilter = mr.CreateFilter,
                DisplayFilter = mr.DisplayFilter,
                Filter = fn i r => mr.Filter i r.nm,
                Sort = Some (fn r1 r2 => mr.Sort r1.nm r2.nm)} 
       in
           {Initialize = m.Initialize,
            Handlers = fn data => case m.Handlers data of
                                      NonNull (mr, _) => doMr mr
                                    | Nullable mr => doMr mr}
       end
        
    con readOnlyState (ts :: (Type * Type * Type * Type)) = (ts.1, ts.3, ts.4)
    fun readOnly [ts] [rest] [nm :: Name] [[nm] ~ rest] name (m : meta ts) : colMeta ([nm = ts.2] ++ rest)
                                                                                     (readOnlyState ts) =
       let
           fun doMr mr = {Header = name,
                          Project = fn r => mr.Initialize r.nm,
                          Update = fn r _ => return r,
                          Display = mr.Display,
                          Edit = mr.Display,
                          Validate = fn _ => return True,
                          CreateFilter = mr.CreateFilter,
                          DisplayFilter = mr.DisplayFilter,
                          Filter = fn i r => mr.Filter i r.nm,
                          Sort = Some (fn r1 r2 => mr.Sort r1.nm r2.nm)}
       in
           {Initialize = m.Initialize,
            Handlers = fn data => case m.Handlers data of
                                      NonNull (mr, _) => doMr mr
                                    | Nullable mr => doMr mr}
       end

    con metaBasic = fn actual_input_filter :: (Type * Type * Type) =>
                       {Display : actual_input_filter.2 -> xbody,
                        Edit : source actual_input_filter.2 -> xbody,
                        Initialize : actual_input_filter.1 -> actual_input_filter.2,
                        InitializeNull : actual_input_filter.2,
                        IsNull : actual_input_filter.2 -> bool,
                        Parse : actual_input_filter.2 -> option actual_input_filter.1,
                        CreateFilter : actual_input_filter.3,
                        DisplayFilter : source actual_input_filter.3 -> xbody,
                        Filter : actual_input_filter.3 -> actual_input_filter.1 -> bool,
                        FilterIsNull : actual_input_filter.3 -> bool,
                        Sort : actual_input_filter.1 -> actual_input_filter.1 -> bool}

    con basicState = source
    con basicFilter = source
    fun basic [ts ::: (Type * Type * Type)] (m : metaBasic ts) : meta (unit, ts.1, basicState ts.2, basicFilter ts.3) =
        {Initialize = return (),
         Handlers = fn () => NonNull (
                             {Display = fn s => <xml><dyn signal={v <- signal s; return (m.Display v)}/></xml>,
                              Edit = m.Edit,
                              Initialize = fn v => source (m.Initialize v),
                              Parse = fn s => v <- signal s; return (m.Parse v),
                              CreateFilter = source m.CreateFilter,
                              DisplayFilter = m.DisplayFilter,
                              Filter = fn f v => f <- signal f;
                                          return (if m.FilterIsNull f then
                                                      True
                                                  else
                                                      m.Filter f v),
                              Sort = m.Sort},
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
                                                       | Some v' => Some (Some v')),
                              CreateFilter = source m.CreateFilter,
                              DisplayFilter = m.DisplayFilter,
                              Filter = fn f v => f <- signal f;
                                          return (if m.FilterIsNull f then
                                                      True
                                                  else
                                                      case v of
                                                          None => False
                                                        | Some v => m.Filter f v),
                              Sort = fn x y =>
                                        case (x, y) of
                                            (None, _) => True
                                          | (Some x', Some y') => m.Sort x' y'
                                          | _ => False})}

    fun nullable [global] [actual] [input] [filter] (m : meta (global, actual, input, filter)) =
        {Initialize = m.Initialize,
         Handlers = fn d => case m.Handlers d of
                                Nullable _ => error <xml>Don't stack calls to Direct.nullable!</xml>
                              | NonNull (_, ho) => Nullable ho}

    type intGlobal = unit
    type intInput = basicState string
    type intFilter = basicFilter string
    val int : meta (intGlobal, int, intInput, intFilter) =
        basic {Display = fn s => <xml>{[s]}</xml>,
               Edit = fn s => <xml><ctextbox size={5} source={s}/></xml>,
               Initialize = fn n => show n,
               InitializeNull = "",
               IsNull = eq "",
               Parse = fn v => read v,
               CreateFilter = "",
               DisplayFilter = fn s => <xml><ctextbox size={5} source={s}/></xml> : xbody,
               Filter = fn s n =>
                           case read s of
                               None => True
                             | Some n' => n' = n,
               FilterIsNull = eq "",
               Sort = le}

    type stringGlobal = unit
    type stringInput = basicState string
    type stringFilter = basicFilter string
    val string : meta (stringGlobal, string, stringInput, stringFilter) =
        basic {Display = fn s => <xml>{[s]}</xml>,
               Edit = fn s => <xml><ctextbox source={s}/></xml>,
               Initialize = fn s => s,
               InitializeNull = "",
               IsNull = eq "",
               Parse = fn s => Some s,
               CreateFilter = "",
               DisplayFilter = fn s => <xml><ctextbox source={s}/></xml> : xbody,
               Filter = fn s n =>
                           case read s of
                               None => True
                             | Some n' => n' = n,
               FilterIsNull = eq "",
               Sort = le}

    type boolGlobal = unit
    type boolInput = basicState bool
    type boolFilter = basicFilter string
    val bool : meta (boolGlobal, bool, boolInput, boolFilter) =
        basic {Display = fn b => <xml>{[b]}</xml>,
               Edit = fn s => <xml><ccheckbox source={s}/></xml>,
               Initialize = fn b => b,
               InitializeNull = False,
               IsNull = fn _ => False,
               Parse = fn b => Some b,
               CreateFilter = "",
               DisplayFilter = fn s => <xml><cselect source={s}>
                 <coption/>
                 <coption value="0">False</coption>
                 <coption value="1">True</coption>
               </cselect></xml> : xbody,
               Filter = fn s b =>
                           case s of
                               "0" => b = False
                             | "1" => b = True
                             | _ => True,
               FilterIsNull = eq "",
               Sort = le}

    functor Foreign (M : sig
                         con row :: {Type}
                         con t :: Type
                         val show_t : show t
                         val read_t : read t
                         val eq_t : eq t
                         val ord_t : ord t
                         val inj_t : sql_injectable t
                         con nm :: Name
                         constraint [nm] ~ row
                         table tab : ([nm = t] ++ row)
                         val render : $([nm = t] ++ row) -> string
                     end) = struct
        open M

        type global = list (t * string)
        type input = source string * option (t * $row)
        type filter = source string

        val getChoices = List.mapQuery (SELECT * FROM tab AS T)
                                       (fn r => (r.T.nm, render r.T))

        fun getChoice k =
            r <- oneRow (SELECT T.{{row}} FROM tab AS T WHERE T.{nm} = {[k]});
            return r.T

        val meta : meta (global, M.t, input, filter) =
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
                            Parse = fn (s, _) => k <- signal s; return (read k : option t),
                            CreateFilter = source "",
                            DisplayFilter = fn s => 
                                               <xml><cselect source={s}>
                                                 <coption/>
                                                 {List.mapX (fn (k, rend) =>
                                                                <xml><coption value={show k}>{[rend]}</coption></xml>)
                                                            choices}
                                               </cselect></xml> : xbody,
                            Filter = fn s k => s <- signal s;
                                        return (case read s : option t of
                                                    None => True
                                                  | Some k' => k' = k),
                            Sort = le},
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
                                                          | Some k => Some (Some k)),
                            CreateFilter = source "",
                            DisplayFilter = fn s => 
                                               <xml><cselect source={s}>
                                                 <coption/>
                                                 <coption value="0">NULL</coption>
                                                 {List.mapX (fn (k, rend) =>
                                                                <xml><coption value={"1" ^ show k}>{[rend]}</coption>
                                                                </xml>)
                                                            choices}
                                               </cselect></xml> : xbody,
                            Filter = fn s ko => s <- signal s;
                                        return (case s of
                                                    "" => True
                                                  | "0" => ko = None
                                                  | _ =>
                                                    case read (String.substring s {Start = 1,
                                                                                   Len = String.length s - 1})
                                                         : option t of
                                                        None => True
                                                      | Some k => ko = Some k),
                            Sort = le})}
    end
end

con computedState = (unit, xbody, unit)
fun computed [row] [t] (_ : show t) name (f : $row -> t) : colMeta row computedState =
    {Initialize = return (),
     Handlers = fn () => {Header = name,
                          Project = fn r => return <xml>{[f r]}</xml>,
                          Update = fn r _ => return r,
                          Display = fn x => x,
                          Edit = fn _ => <xml>...</xml>,
                          Validate = fn _ => return True,
                          CreateFilter = return (),
                          DisplayFilter = fn _ => <xml/>,
                          Filter = fn _ _ => return True,
                          Sort = None}}
fun computedHtml [row] name (f : $row -> xbody) : colMeta row computedState =
    {Initialize = return (),
     Handlers = fn () => {Header = name,
                          Project = fn r => return (f r),
                          Update = fn r _ => return r,
                          Display = fn x => x,
                          Edit = fn _ => <xml>...</xml>,
                          Validate = fn _ => return True,
                          CreateFilter = return (),
                          DisplayFilter = fn _ => <xml/>,
                          Filter = fn _ _ => return True,
                          Sort = None}}

functor Make(M : sig
                 con key :: {Type}
                 con row :: {Type}
                 constraint key ~ row
                 table tab : (key ++ row)

                 val raw : $(map rawMeta (key ++ row))

                 con cols :: {(Type * Type * Type)}
                 val cols : $(map (colMeta (key ++ row)) cols)

                 val keyFolder : folder key
                 val rowFolder : folder row
                 val colsFolder : folder cols

                 con aggregates :: {Type}
                 val aggregates : $(map (aggregateMeta (key ++ row)) aggregates)
                 val aggFolder : folder aggregates

                 val pageLength : option int
             end) = struct
    open Grid.Make(struct
                       fun keyOf r = r --- M.row

                       val list = query (SELECT * FROM {{M.tab}} AS T) (fn r rs => return (r.T :: rs)) []

                       val wholeRow = @Folder.concat ! M.keyFolder M.rowFolder

                       fun ensql [env] (r : $(M.key ++ M.row)) =
                           @map2 [rawMeta] [id] [sql_exp env [] []]
                            (fn [t] meta v => @sql_inject meta.Inj v)
                            wholeRow M.raw r

                       val new =
                           row <- @Monad.mapR _ [rawMeta] [id]
                                   (fn [nm :: Name] [t :: Type] meta => meta.New)
                                   wholeRow M.raw;
                           dml (insert M.tab (ensql row));
                           return row

                       fun selector (r : $M.key) : sql_exp [T = M.key ++ M.row] [] [] bool =
                         @foldR2 [rawMeta] [id]
                          [fn key => rest :: {Type} -> [rest ~ key] => sql_exp [T = key ++ rest] [] [] bool]
                          (fn [nm :: Name] [t :: Type] [key :: {Type}] [[nm] ~ key]
                                           (meta : rawMeta t) (v : t)
                                           (exp : rest :: {Type} -> [rest ~ key] => sql_exp [T = key ++ rest] [] [] bool)
                                           [rest :: {Type}] [rest ~ [nm = t] ++ key] =>
                              (WHERE T.{nm} = {@sql_inject meta.Inj v} AND {exp [[nm = t] ++ rest] !}))
                          (fn [rest :: {Type}] [rest ~ []] => (WHERE TRUE))
                          M.keyFolder (M.raw --- map rawMeta M.row) r
                          [_] !

                       fun save key row =
                           dml (update [M.key ++ M.row]
                                       (ensql row)
                                       M.tab
                                       (selector key))

                       fun delete key =
                           dml (Basis.delete M.tab (selector key))

                       val cols = M.cols

                       val folder = M.colsFolder

                       val aggregates = M.aggregates

                       val aggFolder = M.aggFolder

                       val pageLength = M.pageLength
                   end)
end
