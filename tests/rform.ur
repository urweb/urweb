fun main () : transaction page =
    s <- source "Hi";
    return <xml><body>
      <form>
        <textbox{#A} source={s}/>
      </form>
      <a onclick={set s "NEW AND DIFFERENT"}>Change it up!</a><br/>
      <br/>
      Latest: <dyn signal={s <- signal s; return (cdata s)}/>
    </body></xml>
