open Json

fun main () : transaction page = return <xml><body>
  <pre>{[ fromJson "\"\\\\line 1\\nline 2\"" : string ]}</pre><br/>
  {[fromJson "[1, 2, 3]" : list int]}<br/>
  {[toJson ("hi" :: "bye\"" :: "hehe" :: [])]}
</body></xml>
