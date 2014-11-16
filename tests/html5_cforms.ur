fun dn [a] (_ : show a) (x : source a) : xbody = <xml>
  <dyn signal={v <- signal x; return (txt v)}/>
</xml>

fun main () : transaction page =
    a <- source "";
    b <- source True;
    c <- source "a@b";
    d <- source "";
    e <- source "";
    f <- source "";

    return <xml><body>
      <ctextbox source={a}/>
      <ccheckbox source={b}/>
      <cemail source={c}/>
      <curl source={d}/>
      <ctel source={e}/>
      <csearch source={f}/>

      <hr/>
        
      {dn a};
      {dn b};
      {dn c};
      {dn d};
      {dn e};
      {dn f}
    </body></xml>
