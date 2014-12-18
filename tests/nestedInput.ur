fun main () : transaction page =
    let
        fun handler _ = return <xml/>
    in
        return <xml><body>
          <form>
            <submit action={handler}>Uh oh!</submit>
          </form>
        </body></xml>
    end
