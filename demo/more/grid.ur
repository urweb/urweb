con colMeta' = fn (row :: Type) (t :: Type) =>
                  {Header : string,
                   Project : row -> transaction t,
                   Update : row -> t -> transaction row,
                   Display : t -> xbody,
                   Edit : t -> xbody,
                   Validate : t -> signal bool}

con colMeta = fn (row :: Type) (global_t :: (Type * Type)) =>
                 {Initialize : transaction global_t.1,
                  Handlers : global_t.1 -> colMeta' row global_t.2}                  

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

                 con cols :: {(Type * Type)}
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

    fun make (row : M.row) [t] (m : colMeta' M.row t) : transaction t = m.Project row

    fun makeAll cols row = @@Monad.exec [transaction] _ [map snd M.cols]
                               (map2 [fst] [colMeta M.row] [fn p :: (Type * Type) => transaction p.2]
                                     (fn [p] data meta => make row [_] (meta.Handlers data))
                                     [_] M.folder cols M.cols)
                               (@@Folder.mp [_] [_] M.folder)

    type grid = {Cols : $(map fst M.cols),
                 Rows : Dlist.dlist {Row : source M.row,
                                     Cols : source ($(map snd M.cols)),
                                     Updating : source bool,
                                     Selected : source bool},
                 Selection : source bool}

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

    val createMetas = Monad.mapR [colMeta M.row] [fst]
                           (fn [nm :: Name] [p :: (Type * Type)] meta => meta.Initialize)
                           [_] M.folder M.cols

    val grid =
        cols <- createMetas;
        rows <- Dlist.create;
        sel <- source False;
        return {Cols = cols, Rows = rows, Selection = sel}

    fun sync {Cols = cols, Rows = rows, ...} =
        Dlist.clear rows;
        init <- rpc M.list;
        List.app (addRow cols rows) init

    fun render grid = <xml>
      <table class={tabl}>
        <tr class={tr}>
          <th/> <th/> <th/>
          {foldRX2 [fst] [colMeta M.row] [_]
                   (fn [nm :: Name] [p :: (Type * Type)] [rest :: {(Type * Type)}] [[nm] ~ rest]
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
                                  errors <- Monad.foldR3 [fst] [colMeta M.row] [snd] [fn _ => option string]
                                                         (fn [nm :: Name] [p :: (Type * Type)] [rest :: {(Type * Type)}]
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
                                      row' <- Monad.foldR3 [fst] [colMeta M.row] [snd] [fn _ => M.row]
                                                           (fn [nm :: Name] [t :: (Type * Type)]
                                                                            [rest :: {(Type * Type)}]
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
                                             return (foldRX3 [fst] [colMeta M.row] [snd] [_]
                                                             (fn [nm :: Name] [t :: (Type * Type)]
                                                                              [rest :: {(Type * Type)}]
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
                          end) grid.Rows}

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
