fun main () : transaction page =
    let
        fun handler r = return <xml>
          <head>
            <meta name={blessMeta r.Nam} content={r.Content}/>
            <title>Testing &lt;meta> tags</title>
          </head>
          <body>
            <p>Did it work?</p>
          </body>
        </xml>

        fun handler2 r =
            case checkMeta r.Nam of
                None => error <xml>Oh, that name won't do at all.</xml>
              | Some name =>
                return <xml>
                  <head>
                    <meta name={name} content={r.Content}/>
                    <title>Testing &lt;meta> tags</title>
                  </head>
                  <body>
                    <p>Did it work?</p>
                  </body>
                </xml>
    in
        return <xml>
          <head>
            <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            <title>Testing &lt;meta> tags</title>
          </head>
          <body>
            <p>Did it work?</p>

            <form>
              Name: <textbox{#Nam}/><br/>
              Content: <textbox{#Content}/><br/>
              <submit action={handler}/>
            </form>

            <form>
              Name: <textbox{#Nam}/><br/>
              Content: <textbox{#Content}/><br/>
              <submit action={handler2}/>
            </form>
          </body>
        </xml>
    end
