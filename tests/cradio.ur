fun main () =
s <- source (Some "B");
let
    val onc = v <- get s; alert ("Now it's " ^ show v)
in
    return <xml><body>
      <label>Wilbur <cradio source={s} value="A" onchange={onc}/></label>
      <label>Walbur <cradio source={s} value="B" onchange={onc}/></label>

      Hello, I'm <dyn signal={s <- signal s; return <xml>{[s]}</xml>}/>.
      I'll be your waiter for this evening.
    </body></xml>
end
