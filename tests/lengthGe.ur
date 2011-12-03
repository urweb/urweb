fun main () : transaction page =
    s <- source "";
    return <xml><body>
      <ctextbox source={s}/>
      <dyn signal={v <- signal s;
                   return (txt (strlenGe v 3))}/>
    </body></xml>
