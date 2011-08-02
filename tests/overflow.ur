fun makeList n = if n = 0 then [] else 1 :: makeList (n - 1)

fun doit {N = n} = return <xml><body>{[List.length (makeList (readError n))]}</body></xml>

fun main () =
    return <xml><body>
      <form> <textbox{#N}/> <submit action={doit}/> </form>
    </body></xml>
