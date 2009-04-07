con colMeta = fn t_formT :: (Type * Type) => {
                 Nam : string,
                 Show : t_formT.1 -> xbody,
                 Widget : nm :: Name -> xml form [] [nm = t_formT.2],
                 WidgetPopulated : nm :: Name -> t_formT.1 -> xml form [] [nm = t_formT.2],
                 Parse : t_formT.2 -> t_formT.1,
                 Inject : sql_injectable t_formT.1
                 }
con colsMeta = fn cols :: {(Type * Type)} => $(Top.mapT2T colMeta cols)

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
                 table tab : ([Id = int] ++ mapT2T fstTT cols)

                 val title : string

                 val cols : colsMeta cols
             end) = struct

    open constraints M
    val tab = M.tab

    sequence seq

    fun create (inputs : $(mapT2T sndTT M.cols)) =
        id <- nextval seq;
        () <- dml (insert tab
                          (foldT2R2 [sndTT] [colMeta]
                                    [fn cols => $(mapT2T (fn t :: (Type * Type) =>
                                                             sql_exp [] [] [] t.1) cols)]
                                    (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                                     [[nm] ~ rest] =>
                                     fn input col acc => acc with nm = @sql_inject col.Inject (col.Parse input))
                                    {} [M.cols] inputs M.cols
                           with #Id = (SQL {id})));
        return <xml><body>
          Inserted with ID {[id]}.
        </body></xml>

    fun save (id : int) (inputs : $(mapT2T sndTT M.cols)) =
        () <- dml (update [mapT2T fstTT M.cols]
                          (foldT2R2 [sndTT] [colMeta]
                                    [fn cols => $(mapT2T (fn t :: (Type * Type) =>
                                                             sql_exp [T = [Id = int]
                                                                              ++ mapT2T fstTT M.cols]
                                                                     [] [] t.1) cols)]
                                    (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                                     [[nm] ~ rest] =>
                                     fn input col acc => acc with nm =
                                                                  @sql_inject col.Inject (col.Parse input))
                                    {} [M.cols] inputs M.cols)
                          tab (WHERE T.Id = {id}));
        return <xml><body>
          Saved!
        </body></xml>

    fun update (id : int) =
        fso <- oneOrNoRows (SELECT tab.{{mapT2T fstTT M.cols}} FROM tab WHERE tab.Id = {id});
        case fso : (Basis.option {Tab : $(mapT2T fstTT M.cols)}) of
            None => return <xml><body>Not found!</body></xml>
          | Some fs => return <xml><body><form>
            {foldT2R2 [fstTT] [colMeta] [fn cols :: {(Type * Type)} => xml form [] (mapT2T sndTT cols)]
                      (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                       [[nm] ~ rest] (v : t.1) (col : colMeta t)
                                       (acc : xml form [] (mapT2T sndTT rest)) =>
                          <xml>
                            <li> {cdata col.Nam}: {col.WidgetPopulated [nm] v}</li>
                            {useMore acc}
                          </xml>)
                      <xml/>
                      [M.cols] fs.Tab M.cols}

            <submit action={save id}/>
          </form></body></xml>

    fun delete (id : int) =
        () <- dml (DELETE FROM tab WHERE Id = {id});
        return <xml><body>
          The deed is done.
        </body></xml>

    fun confirm (id : int) = return <xml><body>
      <p>Are you sure you want to delete ID #{[id]}?</p>

      <p><a link={delete id}>I was born sure!</a></p>
    </body></xml>

    fun main () =
        rows <- queryX (SELECT * FROM tab AS T)
                       (fn (fs : {T : $([Id = int] ++ mapT2T fstTT M.cols)}) => <xml>
                         <tr>
                           <td>{[fs.T.Id]}</td>
                           {foldT2RX2 [fstTT] [colMeta] [tr]
                                      (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                                       [[nm] ~ rest] v col => <xml>
                                                         <td>{col.Show v}</td>
                                                       </xml>)
                                      [M.cols] (fs.T -- #Id) M.cols}
                           <td>
                             <a link={update fs.T.Id}>[Update]</a>
                             <a link={confirm fs.T.Id}>[Delete]</a>
                           </td>
                         </tr>
                       </xml>);
        return <xml><head>
          <title>{cdata M.title}</title>
        </head><body>

          <h1>{cdata M.title}</h1>

          <table border={1}>
            <tr>
              <th>ID</th>
              {foldT2RX [colMeta] [tr]
                        (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                         [[nm] ~ rest] col => <xml>
                                           <th>{cdata col.Nam}</th>
                                         </xml>)
                        [M.cols] M.cols}
            </tr>
            {rows}
          </table>

          <br/>

          <form>
            {foldT2R [colMeta] [fn cols :: {(Type * Type)} => xml form [] (mapT2T sndTT cols)]
                     (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                      [[nm] ~ rest] (col : colMeta t) (acc : xml form [] (mapT2T sndTT rest)) => <xml>
                                        <li> {cdata col.Nam}: {col.Widget [nm]}</li>
                                        {useMore acc}
                                      </xml>)
                     <xml/>
                     [M.cols] M.cols}
            
            <submit action={create}/>
          </form>
        </body></xml>

end
