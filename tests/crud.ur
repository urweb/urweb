functor Make(M : sig
        con cols :: {Type}
        constraint [Id] ~ cols
        val tab : sql_table ([Id = int] ++ cols)

        val title : string

        val cols : $(mapTT (fn t => {Show : t -> xbody}) cols)
end) = struct

open constraints M
val tab = M.tab

fun list () =
        rows <- query (SELECT * FROM tab AS T)
                (fn fs acc => return <body>
                        {acc} <tr> <td>{txt _ fs.T.Id}</td> </tr>
                </body>) <body></body>;
        return <html><head>
                <title>List</title>

                </head><body>

                <h1>List</h1>

                <table border={1}>
                <tr> <th>ID</th> </tr>
                {rows}
                </table>
        </body></html>

fun main () : transaction page = return <html><head>
        <title>{cdata M.title}</title>
        </head><body>
        <h1>{cdata M.title}</h1>

        <li> <a link={list ()}>List all rows</a></li>
</body></html>

end
