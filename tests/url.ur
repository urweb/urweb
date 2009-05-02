fun readersChoice r = return <xml><body>
  {case checkUrl r.Url of
       None => <xml>I can't do that, Dave.</xml>
     | Some url => <xml><a href={url}>Your pick, boss</a></xml>}
</body></xml>

fun main () : transaction page = return <xml><body>
  <a href="http://en.wikipedia.org/wiki/Wikipedia:About">Learn</a>
  <br/>

  <form><textbox{#Url}/> <submit action={readersChoice}/></form>
</body></xml>
