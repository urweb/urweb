fun main () : transaction page =
    s <- source "Initial";
    return <xml><body>
      <ctextbox source={s} size=5/>

      <dyn signal={s <- signal s; return (cdata s)}/>
    </body></xml>
