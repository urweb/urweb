fun listHell n =
    if n <= 0 then
        []
    else
        n :: List.append (listHell (n-1)) (listHell (n-1))

fun doit r =
    Transactional.foo;
    return <xml>{[listHell (readError r.N)]}</xml>

fun main () = return <xml><body>
  <form> <textbox{#N}/> <submit action={doit}/> </form>
</body></xml>
