cookie c : {A : string, B : float, C : int}

fun set r =
    setCookie c {Value = {A = r.A, B = readError r.B, C = readError r.C},
                 Expires = None,
                 Secure = False};
    return <xml>Cookie set.</xml>

fun setExp r =
    setCookie c {Value = {A = r.A, B = readError r.B, C = readError r.C},
                 Expires = Some (readError "2012-11-6 00:00:00"),
                 Secure = False};
    return <xml>Cookie set robustly.</xml>

fun delete () =
    clearCookie c;
    return <xml>Cookie cleared.</xml>

fun main () =
    ro <- getCookie c;
    return <xml><body>
      {case ro of
           None => <xml>No cookie set.</xml>
         | Some v => <xml>
           Cookie: A = {[v.A]}, B = {[v.B]}, C = {[v.C]}<br/>
           <form><submit value="Delete" action={delete}/></form>
         </xml>}
      <br/><br/>

      <form>
        A: <textbox{#A}/><br/>
        B: <textbox{#B}/><br/>
        C: <textbox{#C}/><br/>
        <submit action={set}/>
      </form><br/>

      <form>
        <b>Version that expires on November 6, 2012:</b><br/>
        A: <textbox{#A}/><br/>
        B: <textbox{#B}/><br/>
        C: <textbox{#C}/><br/>
        <submit action={setExp}/>
      </form>
    </body></xml>
