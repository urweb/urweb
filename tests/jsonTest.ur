open Json

fun main () : transaction page = return <xml><body>
  <pre>{[ fromJson "\"\\\\line \/ 1\\nline 2\"" : string ]}</pre><br/>
  <pre>{[fromJson "[1, 2, 3]" : list int]}</pre><br/>
  <pre>{[toJson ("hi" :: "bye\"" :: "hehe" :: [])]}</pre>
</body></xml>
