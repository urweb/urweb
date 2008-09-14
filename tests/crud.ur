con colMeta' = fn t :: Type => {Nam : string, Show : t -> xbody}
con colMeta = fn cols :: {Type} => $(Top.mapTT colMeta' cols)

functor Make(M : sig
        con cols :: {Type}
        constraint [Id] ~ cols
        val tab : sql_table ([Id = int] ++ cols)

        val title : string

        val cols : colMeta cols
end) = struct

open constraints M
val tab = M.tab

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
                (fn (fs : {T : $([Id = int] ++ M.cols)}) => <body>
                        <tr>
                                <td>{txt _ fs.T.Id}</td>
                                {foldTRX2 [idT] [colMeta'] [tr]
                                        (fn (nm :: Name) (t :: Type) (rest :: {Type}) =>
                                                [[nm] ~ rest] =>
                                                fn v col => <tr>
                                                        <td>{col.Show v}</td>
                                                </tr>)
                                        [M.cols] (fs.T -- #Id) M.cols}
                                <td><a link={confirm fs.T.Id}>[Delete]</a></td>
                        </tr>
                </body>);
        return <html><head>
                <title>{cdata M.title}</title>

                </head><body>

                <h1>{cdata M.title}</h1>

                <table border={1}>
                <tr>
                        <th>ID</th>
                        {foldTRX [colMeta'] [tr]
                                (fn (nm :: Name) (t :: Type) (rest :: {Type}) =>
                                        [[nm] ~ rest] =>
                                        fn col => <tr>
                                                <th>{cdata col.Nam}</th>
                                        </tr>)
                                [M.cols] M.cols}
                </tr>
                {rows}
                </table>
        </body></html>

end
