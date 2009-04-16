style q
style r

fun main () : transaction page = return <xml>
  <head>
    <link rel="stylesheet" type="text/css" href="http://www.schizomaniac.net/style.css" media="screen"/>
  </head>
  <body>
    Hi.  <span class={q}>And hi <span class={r}>again</span>!</span>
  </body>
</xml>
