con colMeta = fn t_formT :: (Type * Type) => {
                 Nam : string,
                 Show : t_formT.1 -> xbody,
                 Widget : nm :: Name -> xml form [] [nm = t_formT.2],
                 WidgetPopulated : nm :: Name -> t_formT.1 -> xml form [] [nm = t_formT.2],
                 Parse : t_formT.2 -> t_formT.1,
                 Inject : sql_injectable t_formT.1
                 }
con colsMeta = fn cols :: {(Type * Type)} => $(map colMeta cols)

fun default (t ::: Type) (sh : show t) (rd : read t) (inj : sql_injectable t)
            name : colMeta (t, string) =
    {Nam = name,
     Show = txt,
     Widget = fn nm :: Name => <xml><textbox{nm}/></xml>,
     WidgetPopulated = fn (nm :: Name) n =>
                          <xml><textbox{nm} value={show n}/></xml>,
     Parse = readError,
     Inject = _}

val int = default
val float = default
val string = default

fun bool name = {Nam = name,
                 Show = txt,
                 Widget = fn nm :: Name => <xml><checkbox{nm}/></xml>,
                 WidgetPopulated = fn (nm :: Name) b =>
                                      <xml><checkbox{nm} checked={b}/></xml>,
                 Parse = fn x => x,
                 Inject = _}

functor Make(M : sig
                 con cols :: {(Type * Type)}
                 constraint [Id] ~ cols
                 val fl : folder cols

                 val tab : sql_table ([Id = int] ++ map fstTT cols)

                 val title : string

                 val cols : colsMeta cols
             end) = struct

    open constraints M
    val tab = M.tab

    sequence seq

    fun list () =
        rows <- queryX (SELECT * FROM tab AS T)
                       (fn (fs : {T : $([Id = int] ++ map fstTT M.cols)}) => <xml>
                         <tr>
                           <td>{[fs.T.Id]}</td>
                           {foldRX2 [fstTT] [colMeta] [tr]
                                    (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                                     [[nm] ~ rest] v col => <xml>
                                                       <td>{col.Show v}</td>
                                                     </xml>)
                                    [M.cols] M.fl (fs.T -- #Id) M.cols}
                           <td>
                             <a link={upd fs.T.Id}>[Update]</a>
                             <a link={confirm fs.T.Id}>[Delete]</a>
                           </td>
                         </tr>
                       </xml>);
        return <xml>
          <table border={1}>
            <tr>
              <th>ID</th>
              {foldRX [colMeta] [tr]
                        (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                         [[nm] ~ rest] col => <xml>
                                           <th>{cdata col.Nam}</th>
                                         </xml>)
                        [M.cols] M.fl M.cols}
            </tr>
            {rows}
          </table>

          <br/><hr/><br/>

          <form>
            {foldR [colMeta] [fn cols :: {(Type * Type)} => xml form [] (map sndTT cols)]
                   (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                    [[nm] ~ rest] (col : colMeta t) (acc : xml form [] (map sndTT rest)) => <xml>
                                      <li> {cdata col.Nam}: {col.Widget [nm]}</li>
                                      {useMore acc}
                                    </xml>)
                     <xml/>
                     [M.cols] M.fl M.cols}
            
            <submit action={create}/>
          </form>
        </xml>

    and create (inputs : $(map sndTT M.cols)) =
        id <- nextval seq;
        dml (insert tab
                    (foldR2 [sndTT] [colMeta]
                            [fn cols => $(map (fn t :: (Type * Type) =>
                                                  sql_exp [] [] [] t.1) cols)]
                            (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                             [[nm] ~ rest] =>
                             fn input col acc => acc ++ {nm = @sql_inject col.Inject (col.Parse input)})
                            {} [M.cols] M.fl inputs M.cols
                     ++ {Id = (SQL {[id]})}));
        ls <- list ();
        return <xml><body>
          <p>Inserted with ID {[id]}.</p>

          {ls}
        </body></xml>

    and upd (id : int) =
        let
            fun save (inputs : $(map sndTT M.cols)) =
                dml (update [map fstTT M.cols] !
                            (foldR2 [sndTT] [colMeta]
                                    [fn cols => $(map (fn t :: (Type * Type) =>
                                                          sql_exp [T = [Id = int]
                                                                           ++ map fstTT M.cols]
                                                                  [] [] t.1) cols)]
                                    (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                                     [[nm] ~ rest] =>
                                     fn input col acc => acc ++ {nm =
                                                                 @sql_inject col.Inject (col.Parse input)})
                                    {} [M.cols] M.fl inputs M.cols)
                            tab (WHERE T.Id = {[id]}));
                ls <- list ();
                return <xml><body>
                  <p>Saved!</p>

                  {ls}
                </body></xml>
        in
            fso <- oneOrNoRows (SELECT tab.{{map fstTT M.cols}} FROM tab WHERE tab.Id = {[id]});
            case fso : (Basis.option {Tab : $(map fstTT M.cols)}) of
                None => return <xml><body>Not found!</body></xml>
              | Some fs => return <xml><body><form>
                {foldR2 [fstTT] [colMeta] [fn cols :: {(Type * Type)} => xml form [] (map sndTT cols)]
                        (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                         [[nm] ~ rest] (v : t.1) (col : colMeta t)
                                         (acc : xml form [] (map sndTT rest)) =>
                            <xml>
                              <li> {cdata col.Nam}: {col.WidgetPopulated [nm] v}</li>
                              {useMore acc}
                            </xml>)
                        <xml/>
                        [M.cols] M.fl fs.Tab M.cols}

                <submit action={save}/>
              </form></body></xml>
        end

    and confirm (id : int) =
        let
            fun delete () =
                dml (DELETE FROM tab WHERE Id = {[id]});
                ls <- list ();
                return <xml><body>
                  <p>The deed is done.</p>
                  
                  {ls}
                </body></xml>
        in
            return <xml><body>
              <p>Are you sure you want to delete ID #{[id]}?</p>
              
              <p><a link={delete ()}>I was born sure!</a></p>
            </body></xml>
        end    

    and main () =
        ls <- list ();
        return <xml><head>
          <title>{cdata M.title}</title>
        </head><body>

          <h1>{cdata M.title}</h1>

          {ls}
        </body></xml>

end
