fun main () : transaction page = return <xml>
  <head>
    <script code={alert "Hi!"}/>
  </head>
  <body>
    <active code={alert "Bye!"; return <xml/>}/>
  </body>
</xml>
