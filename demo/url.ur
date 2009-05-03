fun yourChoice r = return <xml><body>
  {case checkUrl r.Url of
       None => <xml>You aren't allowed to link to there.</xml>
     | Some url => <xml><a href={url}>Enjoy!</a></xml>}
</body></xml>

fun main () = return <xml><body>
  <a href="http://en.wikipedia.org/wiki/Type_inference">Learn something</a><br/>
  <br/>
  <form>
    URL of your choice: <textbox{#Url}/> <submit action={yourChoice}/>
  </form>
</body></xml>
