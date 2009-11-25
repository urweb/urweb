fun main () : transaction page =
    let
        fun submit r = return <xml/>
    in
        return <xml>
          <body>
            <form>
              <textbox{#Text}/><submit action={submit}/>
            </form>
          </body>
        </xml>
    end


