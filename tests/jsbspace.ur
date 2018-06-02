fun main () : transaction page =
let
  fun onclick (): transaction unit =
    (* this function runs on the client *)
    alert "Some \btext"
in
return <xml>
  <body>
    <button onclick={fn _ => onclick()}>Click me!</button>
  </body>
</xml>
end