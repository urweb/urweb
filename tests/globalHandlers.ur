fun main () : transaction page = return <xml>
  <body onload={onDblclick (fn ev => alert ("ScreenX = " ^ show ev.ScreenX ^ "\nShiftKey = " ^ show ev.ShiftKey));
                onKeypress (fn ev => alert ("KeyCode = " ^ show ev.KeyCode ^ "\nShiftKey = " ^ show ev.ShiftKey))}/>
</xml>
