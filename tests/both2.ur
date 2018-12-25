fun main () : transaction page =
    let
        fun handler r = return <xml/>
    in
        return <xml>
          <body>
            <form>
              <textbox{#Text}/><submit action={handler}/>
            </form>
          </body>
        </xml>
    end
