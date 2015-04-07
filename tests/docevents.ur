fun main () : transaction page = return <xml>
  <body onload={onDblclick (fn _ => alert "Double click");
                onContextmenu (fn _ => alert "Context menu");
                onKeypress (fn k => alert ("Keypress: " ^ show k.KeyCode))}>
    Nothing here.
    </body>
</xml>
