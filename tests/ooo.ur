fun main () : transaction page =
    s <- source "Hi";
    return <xml><body>
      <form>
        <textbox{#A} source={s}/>
        Change it up!
      </form>
    </body></xml>
