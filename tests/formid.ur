fun handler () = return <xml></xml>

fun main () : transaction page =
    id <- fresh;
    return <xml><body>
      <form id={id}>
        <submit action={handler}/>
      </form>
    </body></xml>
