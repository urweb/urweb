fun formbased (): transaction page =
    return <xml>
      <body>
        <form>
          <label>Your name: <textbox{#Nam}/></label>
          <label>Your message: <textarea{#Msg}/></label>
          <label>Delivered on the same day <checkbox{#Sameday}/></label>
          <submit value="Send" action={formbased_handler}/>
        </form>
      </body>
    </xml>

and formbased_handler (r : {Nam : string, Msg : string, Sameday : bool}) : transaction page =
    return <xml>
      <body>
        <p>Oh hello {[r.Nam]}! Great to see you here again!</p>
        <p>Your message was:</p>
        <p>{[r.Msg]}</p>
        <p>Sameday delivery was:</p>
        <p>{[if r.Sameday then "set" else "unset"]}</p>
      </body>
    </xml>

fun say_hi_to (s : string) : transaction page =
return <xml>
  <body>
    <p>It's {[s]} birthday!</p>
  </body>
</xml>

fun optimized_out (): transaction page =
    return <xml>this one is optimized away since it's not referenced in the declarations</xml>

fun main (): transaction page =
    return <xml>
      <body>
        <p>hello</p>
        <p>Say hi to <a link={say_hi_to "JC"}>JC</a></p>
      </body>
</xml>
