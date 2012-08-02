fun handler r =
  vo <- getenv (blessEnvVar r.Nam);
  return <xml><body>
  {case vo of
       None => <xml>Not set</xml>
     | Some v => <xml>Set to: {[v]}</xml>}
</body></xml>

fun main () : transaction page =
  term <- getenv (blessEnvVar "TERM");
  return <xml><body>
    TERM = {case term of
                None => <xml>Nada</xml>
              | Some v => txt v}
                 
    <form>
      What would you like to know?
      <textbox{#Nam}/>
      <submit action={handler}/>
    </form>
  </body></xml>
