fun main () : transaction page = return <xml>
  <body onload={onDblclick (fn ev => alert ("ScreenX = " ^ show ev.ScreenX ^ "\nShiftKey = " ^ show ev.ShiftKey));
                onKeypress (fn ev => alert ("KeyCode = " ^ show ev.KeyCode ^ "\nShiftKey = " ^ show ev.ShiftKey))}/>
</xml>

fun busy () : transaction page = return <xml>
  <body onload={onMouseout (fn _ => alert "OUT!");
                onMouseover (fn _ => alert "OVER!");
                onMousemove (fn _ => alert "MOVE!")}/>
</xml>
