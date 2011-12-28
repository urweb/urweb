style s1
style s2

fun main () : transaction page =
    src <- source s1;
    s <- source "";
    toggle <- source False;
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="http://localhost/test.css"/>
      </head>
      <body>
	<button dynClass={signal src} onclick={set src s2}/>

	<hr/>

	<ctextbox source={s} dynClass={t <- signal toggle;
				       return (if t then s1 else s2)}
        	  onkeyup={fn _ => t <- get toggle; set toggle (not t)}/>
      </body>
    </xml>
