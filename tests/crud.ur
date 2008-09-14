con colMeta = fn t_formT :: (Type * Type) => {
        Nam : string,
        Show : t_formT.1 -> xbody,
        Widget : nm :: Name -> xml form [] [nm = t_formT.2],
        WidgetPopulated : nm :: Name -> t_formT.1 -> xml form [] [nm = t_formT.2],
        Parse : t_formT.2 -> t_formT.1,
        Inject : sql_injectable t_formT.1
}
con colsMeta = fn cols :: {(Type * Type)} => $(Top.mapT2T colMeta cols)

functor Make(M : sig
        con cols :: {(Type * Type)}
        constraint [Id] ~ cols
        val tab : sql_table ([Id = int] ++ mapT2T fstTT cols)

        val title : string

        val cols : colsMeta cols
end) = struct

open constraints M
val tab = M.tab

sequence seq

fun create (inputs : $(mapT2T sndTT M.cols)) =
        id <- nextval seq;
        () <- dml (insert tab (foldT2R2 [sndTT] [colMeta]
                [fn cols => $(mapT2T (fn t :: (Type * Type) =>
                        sql_exp [] [] [] t.1) cols)]
                (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)}) =>
                        [[nm] ~ rest] =>
                        fn input col acc => acc with nm = sql_inject col.Inject (col.Parse input))
                {} [M.cols] inputs M.cols
                with #Id = (SQL {id})));
        return <html><body>
                Inserted with ID {txt _ id}.
        </body></html>

fun save (id : int) (inputs : $(mapT2T sndTT M.cols)) =
        () <- dml (update [mapT2T fstTT M.cols] (foldT2R2 [sndTT] [colMeta]
                [fn cols => $(mapT2T (fn t :: (Type * Type) =>
                        sql_exp [T = [Id = int] ++ mapT2T fstTT M.cols] [] [] t.1) cols)]
                (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)}) =>
                        [[nm] ~ rest] =>
                        fn input col acc => acc with nm = sql_inject col.Inject (col.Parse input))
                {} [M.cols] inputs M.cols)
                tab (WHERE T.Id = {id}));
        return <html><body>
                Saved!
        </body></html>

fun update (id : int) =
        fso <- oneOrNoRows (SELECT tab.{{mapT2T fstTT M.cols}} FROM tab WHERE tab.Id = {id});
        case fso : (Basis.option {Tab : $(mapT2T fstTT M.cols)}) of
          None => return <html><body>Not found!</body></html>
        | Some fs => return <html><body><lform>
                {foldT2R2 [fstTT] [colMeta] [fn cols :: {(Type * Type)} => xml form [] (mapT2T sndTT cols)]
                        (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)}) =>
                                [[nm] ~ rest] =>
                                fn (v : t.1) (col : colMeta t) (acc : xml form [] (mapT2T sndTT rest)) => <lform>
                                        <li> {cdata col.Nam}: {col.WidgetPopulated [nm] v}</li>
                                        {useMore acc}
                                </lform>)
                        <lform></lform>
                        [M.cols] fs.Tab M.cols}

                <submit action={save id}/>
        </lform></body></html>

fun delete (id : int) =
        () <- dml (DELETE FROM tab WHERE Id = {id});
        return <html><body>
                The deed is done.
        </body></html>

fun confirm (id : int) = return <html><body>
        <p>Are you sure you want to delete ID #{txt _ id}?</p>
 
        <p><a link={delete id}>I was born sure!</a></p>
</body></html>

fun main () : transaction page =
        rows <- queryX (SELECT * FROM tab AS T)
                (fn (fs : {T : $([Id = int] ++ mapT2T fstTT M.cols)}) => <body>
                        <tr>
                                <td>{txt _ fs.T.Id}</td>
                                {foldT2RX2 [fstTT] [colMeta] [tr]
                                        (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)}) =>
                                                [[nm] ~ rest] =>
                                                fn v col => <tr>
                                                        <td>{col.Show v}</td>
                                                </tr>)
                                        [M.cols] (fs.T -- #Id) M.cols}
                                <td><a link={update fs.T.Id}>[Update]</a> <a link={confirm fs.T.Id}>[Delete]</a></td>
                        </tr>
                </body>);
        return <html><head>
                <title>{cdata M.title}</title>

                </head><body>

                <h1>{cdata M.title}</h1>

                <table border={1}>
                <tr>
                        <th>ID</th>
                        {foldT2RX [colMeta] [tr]
                                (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)}) =>
                                        [[nm] ~ rest] =>
                                        fn col => <tr>
                                                <th>{cdata col.Nam}</th>
                                        </tr>)
                                [M.cols] M.cols}
                </tr>
                {rows}
                </table>

                <br/>

                <lform>
                        {foldT2R [colMeta] [fn cols :: {(Type * Type)} => xml form [] (mapT2T sndTT cols)]
                                (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)}) =>
                                        [[nm] ~ rest] =>
                                        fn (col : colMeta t) (acc : xml form [] (mapT2T sndTT rest)) => <lform>
                                                <li> {cdata col.Nam}: {col.Widget [nm]}</li>
                                                {useMore acc}
                                        </lform>)
                                <lform></lform>
                                [M.cols] M.cols}

                        <submit action={create}/>
                </lform>
        </body></html>

end
