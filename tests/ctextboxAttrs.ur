fun main () : transaction page =
    s <- source "Initial";
    return <xml><body>
      <ctextbox source={s} onclick={fn ev => alert ("Clicky " ^ show ev.ScreenX)}
                           onkeypress={fn ev => alert ("Code " ^ show ev.KeyCode)}/>
    </body></xml>
