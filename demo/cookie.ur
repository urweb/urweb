cookie c : {A : string, B : float, C : int}

fun set r =
    setCookie c {A = r.A, B = readError r.B, C = readError r.C};
    return <xml>Cookie set.</xml>

fun main () =
    ro <- getCookie c;
    return <xml><body>
      {case ro of
           None => <xml>No cookie set.</xml>
         | Some v => <xml>Cookie: A = {[v.A]}, B = {[v.B]}, C = {[v.C]}</xml>}
      <br/><br/>

      <form>
        A: <textbox{#A}/><br/>
        B: <textbox{#B}/><br/>
        C: <textbox{#C}/><br/>
        <submit action={set}/>
      </form>
    </body></xml>
