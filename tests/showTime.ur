fun main () : transaction page =
    tm <- now;
    s <- source tm;
    return <xml><body>
      <b>Server:</b> {[tm]}<br/>
      <b>Client:</b> <dyn signal={v <- signal s; return (txt v)}/>
      <button value="Recalculate" onclick={tm <- now; set s tm}/>
    </body></xml>
