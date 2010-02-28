style foo

fun main () : transaction page =
    s <- source "Initial";
    return <xml>
      <head>
        <link rel="stylesheet" type="text/css" href="http://localhost/static/style.css"/>
      </head>
      <body>
        <ctextbox source={s} size=5/>
        <ctextbox class={foo} source={s}/>

        <dyn signal={s <- signal s; return (cdata s)}/>
      </body>
    </xml>
