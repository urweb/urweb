con colMeta' = fn (row :: Type) (input :: Type) (filter :: Type) =>
                  {Header : string,
                   Project : row -> transaction input,
                   Update : row -> input -> transaction row,
                   Display : input -> xbody,
                   Edit : input -> xbody,
                   Validate : input -> signal bool,
                   CreateFilter : transaction filter,
                   DisplayFilter : filter -> xbody,
                   Filter : filter -> row -> signal bool,
                   Sort : option (row -> row -> bool)}

con colMeta = fn (row :: Type) (global :: Type, input :: Type, filter :: Type) =>
                 {Initialize : transaction global,
                  Handlers : global -> colMeta' row input filter}                  

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

                 val pageLength : option int
             end) = struct
    style tabl
    style tr
    style th
    style td
    style agg

    fun make (row : M.row) [input] [filter] (m : colMeta' M.row input filter) : transaction input = m.Project row

    fun makeAll cols row = @@Monad.exec [transaction] _ [map snd3 M.cols]
                             (@map2 [fst3] [colMeta M.row] [fn p => transaction (snd3 p)]
                               (fn [p] data meta => make row (meta.Handlers data))
                               M.folder cols M.cols)
                             (@@Folder.mp [_] [_] M.folder)

    type listT = {Row : source M.row,
                  Cols : source ($(map snd3 M.cols)),
                  Updating : source bool,
                  Selected : source bool}

    type grid = {Cols : $(map fst3 M.cols),
                 Rows : Dlist.dlist listT,
                 Selection : source bool,
                 Filters : $(map thd3 M.cols),
                 Sort : source (option (M.row -> M.row -> bool)),
                 Position : source int}

    fun newRow cols row =
        rowS <- source row;
        cols <- makeAll cols row;
        colsS <- source cols;
        ud <- source False;
        sd <- source False;
        return {Row = rowS,
                Cols = colsS,
                Updating = ud,
                Selected = sd}

    fun addRow cols rows row =
        r <- newRow cols row;
        Monad.ignore (Dlist.append rows r)

    val grid =
        cols <- @Monad.mapR _ [colMeta M.row] [fst3]
                 (fn [nm :: Name] [p :: (Type * Type * Type)] meta => meta.Initialize)
                 M.folder M.cols;

        filters <- @Monad.mapR2 _ [colMeta M.row] [fst3] [thd3]
                    (fn [nm :: Name] [p :: (Type * Type * Type)] meta state =>
                        (meta.Handlers state).CreateFilter)
                    M.folder M.cols cols;

        rows <- Dlist.create;
        sel <- source False;
        sort <- source None;
        pos <- source 0;

        return {Cols = cols,
                Rows = rows,
                Selection = sel,
                Filters = filters,
                Sort = sort,
                Position = pos}

    fun sync {Cols = cols, Rows = rows, ...} =
        Dlist.clear rows;
        init <- rpc M.list;
        rs <- List.mapM (newRow cols) init;
        Dlist.replace rows rs

    fun myFilter grid all =
        row <- signal all.Row;
        @foldR3 [colMeta M.row] [fst3] [thd3] [fn _ => M.row -> signal bool]
         (fn [nm :: Name] [p :: (Type * Type * Type)]
                          [rest :: {(Type * Type * Type)}] [[nm] ~ rest]
                          meta state filter combinedFilter row =>
             previous <- combinedFilter row;
             this <- (meta.Handlers state).Filter filter row;
             return (previous && this))
         (fn _ => return True)
         M.folder M.cols grid.Cols grid.Filters row

    fun render (grid : grid) = <xml>
      <table class={tabl}>
        <tr class={tr}>
          <th/> <th/> <th><button value="No sort" onclick={set grid.Sort None}/></th>
          {@mapX2 [fst3] [colMeta M.row] [tr]
            (fn [nm :: Name] [p :: (Type * Type * Type)] [rest :: {(Type * Type * Type)}] [[nm] ~ rest]
                             data (meta : colMeta M.row p) =>
                <xml><th class={th}>
                  {case (meta.Handlers data).Sort of
                       None => txt (meta.Handlers data).Header
                     | sort => <xml><button value={(meta.Handlers data).Header}
                                                      onclick={set grid.Sort sort}/></xml>}
                </th></xml>)
            M.folder grid.Cols M.cols}
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
                                  errors <- @Monad.foldR3 _ [fst3] [colMeta M.row] [snd3] [fn _ => option string]
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
                                             None M.folder grid.Cols M.cols cols;

                                  case errors of
                                      Some s => alert ("Can't save because the following columns have invalid values:\n"
                                                       ^ s)
                                    | None =>
                                      set ud False;
                                      row <- get rowS;
                                      row' <- @Monad.foldR3 _ [fst3] [colMeta M.row] [snd3] [fn _ => M.row]
                                               (fn [nm :: Name] [t :: (Type * Type * Type)]
                                                                [rest :: {(Type * Type * Type)}]
                                                                [[nm] ~ rest] data meta v row' =>
                                                   (meta.Handlers data).Update row' v)
                                               row M.folder grid.Cols M.cols cols;
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
                                             return (@mapX3 [fst3] [colMeta M.row] [snd3] [_]
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
                                                      M.folder grid.Cols M.cols cols)}/>
                                </tr></xml>
                          end)
                      {StartPosition = case M.pageLength of
                                           None => return None
                                         | Some len =>
                                           avail <- Dlist.numPassing (myFilter grid) grid.Rows;
                                           pos <- signal grid.Position;
                                           return (Some (if pos >= avail then
                                                             0
                                                         else
                                                             pos)),
                       MaxLength = return M.pageLength,
                       Filter = myFilter grid,
                       Sort = f <- signal grid.Sort;
                              return (Option.mp (fn f r1 r2 => r1 <- signal r1.Row;
                                                    r2 <- signal r2.Row;
                                                    return (f r1 r2)) f)}
                      grid.Rows}

            <dyn signal={rows <- Dlist.foldl (fn row : listT =>
                                                 @Monad.mapR2 _ [aggregateMeta M.row] [ident] [ident]
                                                  (fn [nm :: Name] [t :: Type] meta acc =>
                                                      Monad.mp (fn v => meta.Step v acc)
                                                               (signal row.Row))
                                                  M.aggFolder M.aggregates)
                                 (@mp [aggregateMeta M.row] [ident]
                                  (fn [t] meta => meta.Initial)
                                  M.aggFolder M.aggregates) grid.Rows;
                         return <xml><tr>
                           <th colspan={3}>Aggregates</th>
                           {@mapX2 [aggregateMeta M.row] [ident] [_]
                             (fn [nm :: Name] [t :: Type] [rest :: {Type}] [[nm] ~ rest] meta acc =>
                                 <xml><td class={agg}>{meta.Display acc}</td></xml>)
                             M.aggFolder M.aggregates rows}
                         </tr></xml>}/>

              <tr><th colspan={3}>Filters</th>
                {@mapX3 [colMeta M.row] [fst3] [thd3] [_]
                  (fn [nm :: Name] [p :: (Type * Type * Type)] [rest :: {(Type * Type * Type)}] [[nm] ~ rest]
                                   meta state filter => <xml><td>{(meta.Handlers state).DisplayFilter filter}</td></xml>)
                  M.folder M.cols grid.Cols grid.Filters}
              </tr>
          </table>

          {case M.pageLength of
               None => <xml/>
             | Some plen => <xml>
               <dyn signal={avail <- Dlist.numPassing (myFilter grid) grid.Rows;
                            return (if avail <= plen then
                                        <xml/>
                                    else
                                        let
                                            val numPages = avail / plen
                                            val numPages = if numPages * plen < avail then
                                                               numPages + 1
                                                           else
                                                               numPages

                                            fun pages n =
                                                if n * plen >= avail then
                                                    <xml/>
                                                else
                                                    <xml>
                                                      <dyn signal={pos <- signal grid.Position;
                                                                   return (if n * plen = pos then
                                                                               <xml><b>{[n + 1]}</b></xml>
                                                                           else
                                                                               <xml>
                                                                                 <button value={show (n + 1)}
                                                                                         onclick={set grid.Position
                                                                                                      (n * plen)
                                                                                                 }/></xml>)}/>
                                                      {if (n + 1) * plen >= avail then <xml/> else <xml>|</xml>}
                                                      {pages (n + 1)}
                                                    </xml>
                                        in
                                            <xml><p><b>Pages:</b> {pages 0}</p></xml>
                                        end)}/>
               </xml>}
          
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
