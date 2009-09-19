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

structure Direct : sig
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

    con meta = fn global_actual_input_filter :: (Type * Type * Type * Type) =>
                  {Initialize : transaction global_actual_input_filter.1,
                   Handlers : global_actual_input_filter.1
                              -> metaBoth global_actual_input_filter.2 global_actual_input_filter.3
                                          global_actual_input_filter.4}

    con editableState :: (Type * Type * Type * Type) -> (Type * Type * Type)
    val editable : ts ::: (Type * Type * Type * Type) -> rest ::: {Type}
                   -> nm :: Name -> [[nm] ~ rest] => string -> meta ts
                                                     -> colMeta ([nm = ts.2] ++ rest)
                                                                (editableState ts)

    con readOnlyState :: (Type * Type * Type * Type) -> (Type * Type * Type)
    val readOnly : ts ::: (Type * Type * Type * Type) -> rest ::: {Type}
                   -> nm :: Name -> [[nm] ~ rest] => string -> meta ts
                                                     -> colMeta ([nm = ts.2] ++ rest)
                                                                (readOnlyState ts)

    val nullable : global ::: Type -> actual ::: Type -> input ::: Type -> filter ::: Type
                   -> meta (global, actual, input, filter)
                   -> meta (global, option actual, input, filter)

    type intGlobal
    type intInput
    type intFilter
    val int : meta (intGlobal, int, intInput, intFilter)

    type stringGlobal
    type stringInput
    type stringFilter
    val string : meta (stringGlobal, string, stringInput, stringFilter)

    type boolGlobal
    type boolInput
    type boolFilter
    val bool : meta (boolGlobal, bool, boolInput, boolFilter)

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
                     end) : sig
        type global
        type input
        type filter
        val meta : meta (global, M.t, input, filter)
    end
end

con computedState :: (Type * Type * Type)
val computed : row ::: {Type} -> t ::: Type -> show t
               -> string -> ($row -> t) -> colMeta row computedState
val computedHtml : row ::: {Type} -> string -> ($row -> xbody) -> colMeta row computedState

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
             end) : sig
    type grid

    val grid : transaction grid
    val sync : grid -> transaction unit
    val render : grid -> xbody

    val showSelection : grid -> source bool
    val selection : grid -> signal (list ($(M.key ++ M.row)))
end
