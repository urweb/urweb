table t1 : {A : int, B : string, C : float, D : bool}

fun display (q : sql_query [T1 = [A = int, B = string, C = float, D = bool]] []) =
        s <- query q
                (fn fs _ => return (Some fs.T1))
                None;
        return <html><body>
                {case s of
                  None => cdata "Row not found."
                | Some s =>
                        <body>
                                A: {cdata (show _ s.A)}<br/>
                                B: {cdata (show _ s.B)}<br/>
                                C: {cdata (show _ s.C)}<br/>
                                D: {cdata (show _ s.D)}<br/>
                        </body>}
        </body></html>

fun lookupA (inp : {A : string}) =
        display (SELECT * FROM t1 WHERE t1.A = {readError _ inp.A})

fun lookupB (inp : {B : string}) =
        display (SELECT * FROM t1 WHERE t1.B = {inp.B})

fun lookupC (inp : {C : string}) =
        display (SELECT * FROM t1 WHERE t1.C = {readError _ inp.C})

fun lookupD (inp : {D : string}) =
        display (SELECT * FROM t1 WHERE t1.D = {readError _ inp.D})

fun main () : transaction page = return <html><body>
        <lform>
                A: <textbox{#A}/>
                <submit action={lookupA}/>
        </lform>

        <lform>
                B: <textbox{#B}/>
                <submit action={lookupB}/>
        </lform>

        <lform>
                C: <textbox{#C}/>
                <submit action={lookupC}/>
        </lform>

        <lform>
                D: <textbox{#D}/>
                <submit action={lookupD}/>
        </lform>
</body></html>
