fun main () =
    s <- source "";
    return <xml><body onresize={set s "Resize"}>
      <button onblur={set s "Blur 1"} onfocus={set s "Focus 1"} onclick={set s "Click 1"}/>
      <button onblur={set s "Blur 2"} onfocus={set s "Focus 2"} onclick={set s "Click 2"}/>
      <span onclick={set s "Click"} ondblclick={set s "Double-click"}>SPAN</span>
      <span onmousedown={set s "Mouse down"} onmouseup={set s "Mouse up"}>SPAN</span>
      <span onmouseout={set s "Mouse out"} onmouseover={set s "Mouse over"}>SPAN</span>
      <span onmousemove={set s "Mouse move"}>SPAN</span>
      <button onkeydown={set s "Key down"} onkeyup={set s "Key up"}/>
      <button onkeypress={set s "Key press"}/>
      <br/>
      <br/>

      <dyn signal={s <- signal s; return <xml>{[s]}</xml>}/>
    </body></xml>
