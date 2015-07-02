fun main () : transaction page = return <xml>
  <body onclick={fn _ => alert "You clicked the body."}
        onkeyup={fn _ => alert "Key"}>
    <p>Text</p>
  </body>
</xml>
