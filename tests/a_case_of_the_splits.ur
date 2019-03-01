fun newCounter () : transaction xbody =
  x <- source 0;
  return <xml>
    <dyn signal={n <- signal x; return <xml>{[n]}</xml>}/>
  </xml>

fun main () : transaction page =
  ls <- source ([] : list xbody);
  return <xml>
    <body>
      <button value="Add" onclick={fn _ =>
        l <- get ls;
        c <- newCounter ();
        set ls (c :: l)}/>
      <dyn signal={l <- signal ls; return <xml>{[l]}</xml>}/>
    </body>
  </xml>
