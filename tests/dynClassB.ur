style style1
style style2

fun main () : transaction page =
    toggle <- source False;
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/style.css"/>
      </head>
      <body dynClass={b <- signal toggle;
                      return (if b then style1 else style2)}
            dynStyle={b <- signal toggle;
                      return (if b then STYLE "margin: 100px" else STYLE "")}>
        Body
	<button onclick={fn _ => b <- get toggle; set toggle (not b)}>TOGGLE</button>
      </body>
    </xml>
