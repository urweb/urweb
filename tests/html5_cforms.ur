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
    g <- source (Some 1.0);
    h <- source (Some 1.0);
    i <- source "#CCCCCC";
    j <- source "2014/11/16";
    k <- source "2014/11/16 12:30:45";
    l <- source "2014/11/16 12:30:45";
    m <- source "2014/11";
    n <- source "2014-W7";
    o <- source "12:30:45";

    return <xml><body>
      <ctextbox source={a}/>
      <ccheckbox source={b}/>
      <cemail source={c}/>
      <curl source={d}/>
      <ctel source={e}/>
      <csearch source={f}/>
      <cnumber source={g} min={-10.0} max={10.0} step={0.5}/>
      <crange source={h} min={-10.0} max={10.0}/>
      <ccolor source={i}/>
      <cdate source={j}/>
      <cdatetime source={k}/>
      <cdatetime-local source={l}/>
      <cmonth source={m}/>
      <cweek source={n}/>
      <ctime source={o}/>

      <hr/>
        
      {dn a};
      {dn b};
      {dn c};
      {dn d};
      {dn e};
      {dn f};
      {dn g};
      {dn h};
      {dn i};
      {dn j};
      {dn k};
      {dn l};
      {dn m};
      {dn n};
      {dn o}
    </body></xml>
