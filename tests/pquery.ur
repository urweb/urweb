table t1 : {A : int, B : string, C : float}

fun lookup (inp : {B : string}) =
        s <- query (SELECT * FROM t1 WHERE t1.B = {inp.B})
                (fn fs _ => return fs.T1)
                {A = 0, B = "Couldn't find it!", C = 0.0};
        return <html><body>
                A: {cdata (show _ s.A)}<br/>
                B: {cdata (show _ s.B)}<br/>
                C: {cdata (show _ s.C)}<br/>
        </body></html>

fun main () : transaction page = return <html><body>
        <lform>
                B: <textbox{#B}/>
                <submit action={lookup}/>
        </lform>
</body></html>
