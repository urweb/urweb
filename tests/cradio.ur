fun main () =
s <- source (Some "B");
r <- source None;
let
    val onc = v <- get s; alert ("Now it's " ^ show v)
    val onc_r = v <- get r; alert ("Changed to " ^ show v)
in
    return <xml><body>
      <h1>First group</h1>

      <label>Wilbur <cradio source={s} value="A" onchange={onc}/></label>
      <label>Walbur <cradio source={s} value="B" onchange={onc}/></label>

      <div>
	Hello, I'm <dyn signal={s <- signal s; return <xml>{[s]}</xml>}/>. I'll be your waiter for this evening.
      </div>

      <h1>Second group</h1>

      <label>X <cradio source={r} value="X" onchange={onc_r}/></label>
      <label>Y <cradio source={r} value="Y" onchange={onc_r}/></label>
      <label>Z <cradio source={r} value="Z" onchange={onc_r}/></label>

      <div>Value: <dyn signal={r <- signal r; return <xml>{[r]}</xml>}/></div>
    </body></xml>
end
