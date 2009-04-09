val url = "http://www.yahoo.com/"

fun readersChoice r = return <xml><body>
  <a href={bless r.Url}>Your pick, boss</a>
</body></xml>

fun main () : transaction page = return <xml><body>
  <a href="http://www.google.com/">Google!</a>
  <a href={bless url}>Yahoo!</a><br/>
  <br/>

  <form><textbox{#Url}/> <submit action={readersChoice}/></form>
</body></xml>
