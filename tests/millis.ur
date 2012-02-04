fun diffThem tm =
    tm' <- now;
    return <xml><body>
      Diff: {[diffInMilliseconds tm tm']}
    </body></xml>

fun main () : transaction page =
    tm <- now;

    return <xml><body>
      Now: {[toMilliseconds tm]}<br/>
      <a link={diffThem tm}>Diff</a><br/>
      
      <button onclick={tm' <- now;
                       alert ("Now: " ^ show (toMilliseconds tm'));
                       alert ("Diff: " ^ show (diffInMilliseconds tm tm'))}/>
    </body></xml>
