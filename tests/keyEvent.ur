fun main () : transaction page = return <xml><body>
  <ctextbox onkeypress={fn ev => alert ("KeyCode = " ^ show ev.KeyCode
                                        ^ "\nCtrlKey = " ^ show ev.CtrlKey
                                        ^ "\nShiftKey = " ^ show ev.ShiftKey
                                        ^ "\nAltKey = " ^ show ev.AltKey
                                        ^ "\nMetaKey = " ^ show ev.MetaKey)}/>
</body></xml>
