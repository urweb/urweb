fun pageA () = return <xml>
  <head>
    <title>A</title>
  </head>
  <body>
    <form>
      <table>
        <tr>
          <td>Forename:</td>
          <td><textbox{#Forename}/></td>
        </tr>
        <tr>
          <td>Enter a Surname?</td>
          <td><checkbox{#EnterSurname}/></td>
        </tr>
      </table>
      <submit action={fromA} />
    </form>
  </body>
</xml>

and fromA r =
    let
        val forename = r.Forename

        fun pageB () = return <xml>
          <head>
            <title>B</title>
          </head>
          <body>
            <form>
              Surname:
              <textbox{#Surname}/>
              <submit action={pageC'} />
            </form>
            <a link={pageA ()}>Previous</a>
          </body>
        </xml>

        and pageC' r = pageC (Some r.Surname)

        and pageC surname = return <xml>
          <head>
            <title>C</title>
          </head>
          <body>
            <p>Hello {[forename]}{case surname of
                                      None => <xml></xml>
                                    | Some s => <xml> {[s]}</xml>}</p>
            {case surname of
                 None => <xml><a link={pageA ()}>Previous</a></xml>
               | Some _ => <xml><a link={pageB ()}>Previous</a></xml>}
          </body>
        </xml>
    in
        if r.EnterSurname then
            pageB ()
        else
            pageC None
    end

fun main () = pageA ()
