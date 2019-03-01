fun main () : transaction page =
    s <- source True;
    t <- source 1;
    return <xml><body><ccheckbox source={s} onclick={fn _ => set t 3}/>
      <dyn signal={s <- signal s;
                   t <- signal t;
                   return <xml>{[s]} {[t]}</xml>}/>
    </body></xml>
