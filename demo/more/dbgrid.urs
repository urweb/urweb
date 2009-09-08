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

structure Direct : sig
    con meta = fn global_actual_input :: (Type * Type * Type) =>
                  {Initialize : transaction global_actual_input.1,
                   Handlers : global_actual_input.1
                              -> {Display : global_actual_input.3 -> xbody,
                                  Edit : global_actual_input.3 -> xbody,
                                  Initialize : global_actual_input.2 -> transaction global_actual_input.3,
                                  Parse : global_actual_input.3 -> signal (option global_actual_input.2)}}

    con editableState :: (Type * Type * Type) -> (Type * Type)
    val editable : ts ::: (Type * Type * Type) -> rest ::: {Type}
                   -> nm :: Name -> [[nm] ~ rest] => string -> meta ts
                                                     -> colMeta ([nm = ts.2] ++ rest)
                                                                (editableState ts)

    con readOnlyState :: (Type * Type * Type) -> (Type * Type)
    val readOnly : ts ::: (Type * Type * Type) -> rest ::: {Type}
                   -> nm :: Name -> [[nm] ~ rest] => string -> meta ts
                                                     -> colMeta ([nm = ts.2] ++ rest)
                                                                (readOnlyState ts)

    type intGlobal
    type intInput
    val int : meta (intGlobal, int, intInput)

    type stringGlobal
    type stringInput
    val string : meta (stringGlobal, string, stringInput)

    type boolGlobal
    type boolInput
    val bool : meta (boolGlobal, bool, boolInput)

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
                     end) : sig
        con global :: Type
        con input :: Type
        val meta : meta (global, M.t, input)
    end
end

con computedState :: (Type * Type)
val computed : row ::: {Type} -> t ::: Type -> show t
               -> string -> ($row -> t) -> colMeta row computedState
val computedHtml : row ::: {Type} -> string -> ($row -> xbody) -> colMeta row computedState

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
             end) : sig
    type grid

    val grid : transaction grid
    val sync : grid -> transaction unit
    val render : grid -> xbody
end
