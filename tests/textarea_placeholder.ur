fun lame _ = return <xml/>

fun main () =
    s <- source "";
    return <xml><body>
      <form>
        <textarea{#Text} placeholder="Type something here."/>
        <submit action={lame}/>
      </form>

      <ctextarea source={s} placeholder="Absolutely don't type something here."/>
    </body></xml>
