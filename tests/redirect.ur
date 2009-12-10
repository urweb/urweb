fun other () = redirect (bless "http://www.google.com/")

fun further () = case checkUrl "http://www.google.com/" of
                     None => return <xml>Darn.</xml>
                   | Some url => redirect url

fun failing () = case checkUrl "http://www.yahoo.com/" of
                     None => return <xml>Darn.</xml>
                   | Some url => redirect url

fun main () = return <xml><body>
  <a link={other ()}>Go there</a><br/>
  <a link={further ()}>Go also there</a><br/>
  <a link={failing ()}>Fail there</a>
</body></xml>
