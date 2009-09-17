con colMeta' = fn (row :: Type) (input :: Type) (filter :: Type) =>
                  {Header : string,
                   Project : row -> transaction input,
                   Update : row -> input -> transaction row,
                   Display : input -> xbody,
                   Edit : input -> xbody,
                   Validate : input -> signal bool,
                   CreateFilter : transaction filter,
                   DisplayFilter : filter -> xbody,
                   Filter : filter -> row -> signal bool}
                  
con colMeta = fn (row :: Type) (global_input_filter :: (Type * Type * Type)) =>
                 {Initialize : transaction global_input_filter.1,
                  Handlers : global_input_filter.1 -> colMeta' row global_input_filter.2 global_input_filter.3}

con aggregateMeta = fn (row :: Type) (acc :: Type) =>
                       {Initial : acc,
                        Step : row -> acc -> acc,
                        Display : acc -> xbody}

functor Make(M : sig
                 type row
                 type key
                 val keyOf : row -> key

                 val list : transaction (list row)
                 val new : transaction row
                 val save : key -> row -> transaction unit
                 val delete : key -> transaction unit

                 con cols :: {(Type * Type * Type)}
                 val cols : $(map (colMeta row) cols)

                 val folder : folder cols

                 con aggregates :: {Type}
                 val aggregates : $(map (aggregateMeta row) aggregates)
                 val aggFolder : folder aggregates
             end) = struct
    style tabl
    style tr
    style th
    style td
    style agg

    fun make (row : M.row) [input] [filter] (m : colMeta' M.row input filter) : transaction input = m.Project row

    fun makeAll cols row = @@Monad.exec [transaction] _ [map snd3 M.cols]
                               (map2 [fst3] [colMeta M.row] [fn p => transaction (snd3 p)]
                                     (fn [p] data meta => make row [_] [_] (meta.Handlers data))
                                     [_] M.folder cols M.cols)
                               (@@Folder.mp [_] [_] M.folder)

    type grid = {Cols : $(map fst3 M.cols),
                 Rows : Dlist.dlist {Row : source M.row,
                                     Cols : source ($(map snd3 M.cols)),
                                     Updating : source bool,
                                     Selected : source bool},
                 Selection : source bool,
                 Filters : $(map thd3 M.cols)}

    fun addRow cols rows row =
        rowS <- source row;
        cols <- makeAll cols row;
        colsS <- source cols;
        ud <- source False;
        sd <- source False;
        Monad.ignore (Dlist.append rows {Row = rowS,
                                         Cols = colsS,
                                         Updating = ud,
                                         Selected = sd})

    val grid =
        cols <- Monad.mapR [colMeta M.row] [fst3]
                           (fn [nm :: Name] [p :: (Type * Type * Type)] meta => meta.Initialize)
                           [_] M.folder M.cols;

        filters <- Monad.mapR2 [colMeta M.row] [fst3] [thd3]
                               (fn [nm :: Name] [p :: (Type * Type * Type)] meta state =>
                                   (meta.Handlers state).CreateFilter)
                               [_] M.folder M.cols cols;

        rows <- Dlist.create;
        sel <- source False;

        return {Cols = cols,
                Rows = rows,
                Selection = sel,
                Filters = filters}

    fun sync {Cols = cols, Rows = rows, ...} =
        Dlist.clear rows;
        init <- rpc M.list;
        List.app (addRow cols rows) init

    fun render grid = <xml>
      <table class={tabl}>
        <tr class={tr}>
          <th/> <th/> <th/>
          {foldRX2 [fst3] [colMeta M.row] [_]
                   (fn [nm :: Name] [p :: (Type * Type * Type)] [rest :: {(Type * Type * Type)}] [[nm] ~ rest]
                                    data (meta : colMeta M.row p) =>
                       <xml><th class={th}>{[(meta.Handlers data).Header]}</th></xml>)
                   [_] M.folder grid.Cols M.cols}
        </tr>

        {Dlist.render (fn {Row = rowS, Cols = colsS, Updating = ud, Selected = sd} pos =>
                          let
                              val delete =
                                  Dlist.delete pos;
                                  row <- get rowS;
                                  rpc (M.delete (M.keyOf row))

                              val update = set ud True

                              val cancel =
                                  set ud False;
                                  row <- get rowS;
                                  cols <- makeAll grid.Cols row;
                                  set colsS cols
                                  
                              val save =
                                  cols <- get colsS;
                                  errors <- Monad.foldR3 [fst3] [colMeta M.row] [snd3] [fn _ => option string]
                                                         (fn [nm :: Name] [p :: (Type * Type * Type)] [rest :: {(Type * Type * Type)}]
                                                                          [[nm] ~ rest] data meta v errors =>
                                                             b <- current ((meta.Handlers data).Validate v);
                                                             return (if b then
                                                                         errors
                                                                     else
                                                                         case errors of
                                                                             None => Some ((meta.Handlers data).Header)
                                                                           | Some s => Some ((meta.Handlers data).Header
                                                                                             ^ ", " ^ s)))
                                                         None [_] M.folder grid.Cols M.cols cols;

                                  case errors of
                                      Some s => alert ("Can't save because the following columns have invalid values:\n"
                                                       ^ s)
                                    | None =>
                                      set ud False;
                                      row <- get rowS;
                                      row' <- Monad.foldR3 [fst3] [colMeta M.row] [snd3] [fn _ => M.row]
                                                           (fn [nm :: Name] [t :: (Type * Type * Type)]
                                                                            [rest :: {(Type * Type * Type)}]
                                                                            [[nm] ~ rest] data meta v row' =>
                                                               (meta.Handlers data).Update row' v)
                                                           row [_] M.folder grid.Cols M.cols cols;
                                      rpc (M.save (M.keyOf row) row');
                                      set rowS row';

                                      cols <- makeAll grid.Cols row';
                                      set colsS cols
                          in
                              <xml><tr class={tr}>
                                <td>
                                  <dyn signal={b <- signal grid.Selection;
                                               return (if b then
                                                           <xml><ccheckbox source={sd}/></xml>
                                                       else
                                                           <xml/>)}/>
                                </td>

                                <td>
                                  <dyn signal={b <- signal ud;
                                               return (if b then
                                                           <xml><button value="Save" onclick={save}/></xml>
                                                       else
                                                           <xml><button value="Update" onclick={update}/></xml>)}/>
                                </td>

                                <td><dyn signal={b <- signal ud;
                                                 return (if b then
                                                             <xml><button value="Cancel" onclick={cancel}/></xml>
                                                         else
                                                             <xml><button value="Delete" onclick={delete}/></xml>)}/>
                                </td>

                                <dyn signal={cols <- signal colsS;
                                             return (foldRX3 [fst3] [colMeta M.row] [snd3] [_]
                                                             (fn [nm :: Name] [t :: (Type * Type * Type)]
                                                                              [rest :: {(Type * Type * Type)}]
                                                                              [[nm] ~ rest] data meta v =>
                                                                 <xml><td class={td}>
                                                                   <dyn signal={b <- signal ud;
                                                                                return (if b then
                                                                                            (meta.Handlers data).Edit v
                                                                                        else
                                                                                            (meta.Handlers data).Display
                                                                                                                v)}/>
                                                                   <dyn signal={b <- signal ud;
                                                                                if b then
                                                                                    valid <-
                                                                                    (meta.Handlers data).Validate v;
                                                                                    return (if valid then
                                                                                                <xml/>
                                                                                            else
                                                                                                <xml>!</xml>)
                                                                                else
                                                                                    return <xml/>}/>
                                                                 </td></xml>)
                                                             [_] M.folder grid.Cols M.cols cols)}/>
                                </tr></xml>
                          end)
                      {Filter = fn all =>
                                   row <- signal all.Row;
                                   foldR3 [colMeta M.row] [fst3] [thd3] [fn _ => M.row -> signal bool]
                                          (fn [nm :: Name] [p :: (Type * Type * Type)]
                                                           [rest :: {(Type * Type * Type)}] [[nm] ~ rest]
                                                           meta state filter combinedFilter row =>
                                              previous <- combinedFilter row;
                                              this <- (meta.Handlers state).Filter filter row;
                                              return (previous && this))
                                          (fn _ => return True)
                                          [_] M.folder M.cols grid.Cols grid.Filters row,
                       Sort = return None}
                      grid.Rows}

            <dyn signal={rows <- Dlist.foldl (fn row => Monad.mapR2 [aggregateMeta M.row] [id] [id]
                                                                    (fn [nm :: Name] [t :: Type] meta acc =>
                                                                        Monad.mp (fn v => meta.Step v acc)
                                                                                 (signal row.Row))
                                                                    [_] M.aggFolder M.aggregates)
                                 (mp [aggregateMeta M.row] [id]
                                  (fn [t] meta => meta.Initial)
                                  [_] M.aggFolder M.aggregates) grid.Rows;
                         return <xml><tr>
                           <th colspan={3}>Aggregates</th>
                           {foldRX2 [aggregateMeta M.row] [id] [_]
                                    (fn [nm :: Name] [t :: Type] [rest :: {Type}] [[nm] ~ rest] meta acc =>
                                        <xml><td class={agg}>{meta.Display acc}</td></xml>)
                                    [_] M.aggFolder M.aggregates rows}
                         </tr></xml>}/>

              <tr><th colspan={3}>Filters</th>
                {foldRX3 [colMeta M.row] [fst3] [thd3] [_]
                 (fn [nm :: Name] [p :: (Type * Type * Type)] [rest :: {(Type * Type * Type)}] [[nm] ~ rest]
                                  meta state filter => <xml><td>{(meta.Handlers state).DisplayFilter filter}</td></xml>)
                 [_] M.folder M.cols grid.Cols grid.Filters}
              </tr>
          </table>
          
          <button value="New row" onclick={row <- rpc M.new;
                                           addRow grid.Cols grid.Rows row}/>
          <button value="Refresh" onclick={sync grid}/>
    </xml>

    fun showSelection grid = grid.Selection

    fun selection grid = Dlist.foldl (fn {Row = rowS, Selected = sd, ...} ls =>
                                         sd <- signal sd;
                                         if sd then
                                             row <- signal rowS;
                                             return (row :: ls)
                                         else
                                             return ls) [] grid.Rows
end
