fun main () : transaction page =
    date <- source "2011/11/26 10:08:42";
    format <- source "";
    return <xml><body>
      <ctextbox source={date}/>
      <ctextbox source={format}/>
      <dyn signal={d <- signal date;
                   f <- signal format;
                   return (case read d of
                               None => <xml/>
                             | Some d => <xml>{[timef f d]}</xml>)}/>
    </body></xml>
