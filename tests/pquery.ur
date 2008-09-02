table t1 : {A : int, B : string, C : float}

fun lookup (inp : {B : string}) =
        s <- query (SELECT t1.B FROM t1 WHERE t1.B = {inp.B})
                (fn fs _ => return fs.T1.B)
                "Couldn't find it!";
        return <html><body>
                Result: {cdata s}
        </body></html>

fun main () : transaction page = return <html><body>
        <lform>
                B: <textbox{#B}/>
                <submit action={lookup}/>
        </lform>
</body></html>
