val show_mouseButton = mkShow (fn b => case b of
                                           Left => "Left"
                                         | Middle => "Middle"
                                         | Right => "Right")

fun main () : transaction page = return <xml><body>
  <button onclick={fn ev => alert ("ScreenX = " ^ show ev.ScreenX
                                   ^ "\nScreenY = " ^ show ev.ScreenY
                                   ^ "\nClientX = " ^ show ev.ClientX
                                   ^ "\nClientY = " ^ show ev.ClientY
                                   ^ "\nOffsetX = " ^ show ev.OffsetX
                                   ^ "\nOffsetY = " ^ show ev.OffsetY
                                   ^ "\nCtrlKey = " ^ show ev.CtrlKey
                                   ^ "\nShiftKey = " ^ show ev.ShiftKey
                                   ^ "\nAltKey = " ^ show ev.AltKey
                                   ^ "\nMetaKey = " ^ show ev.MetaKey
                                   ^ "\nButton = " ^ show ev.Button)}/>
</body></xml>
