functor Make(M : sig
                 type t
                 val read_t : read t
                 val show_t : show t
                 val num_t : num t
             end) = struct
    fun calculate (n1, n2) = return <xml><body>
      {[readError n1 / readError n2 : M.t]}<br/>
      {[readError n1 % readError n2 : M.t]}<br/>
    </body></xml>

    fun main () =
        s1 <- source "";
        s2 <- source "";
        s3 <- source "";
        s4 <- source "";
        return <xml><body>
          <h1>Client-side</h1>

          <ctextbox source={s1}/> / <ctextbox source={s2}/>
          <button value="=" onclick={n1 <- get s1;
                                     n2 <- get s2;
                                     set s3 (show (readError n1 / readError n2 : M.t));
                                     set s4 (show (readError n1 % readError n2 : M.t))}/>
          <dyn signal={n <- signal s3; return (txt n)}/>,
          <dyn signal={n <- signal s4; return (txt n)}/>

          <h1>Server-side</h1>

          <form>
            <textbox{#1}/> / <textbox{#2}/>
            <submit value="=" action={calculate}/>
          </form>
        </body></xml>
end

structure Int = Make(struct type t = int end)
structure Float = Make(struct type t = float end)

fun main () : transaction page = return <xml><body>
  <li><a link={Int.main ()}>Int</a></li>
  <li><a link={Float.main ()}>Float</a></li>
</body></xml>
