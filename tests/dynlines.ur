datatype lines = End | Line of source lines

type t = { Head : source lines, Tail : source (source lines) }

val create =
    head <- source End;
    tail <- source head;
    return {Head = head, Tail = tail}

fun renderL lines =
    case lines of
        End => <xml/>
      | Line linesS => <xml>X<br/><dyn signal={renderS linesS}/></xml>

and renderS linesS =
    lines <- signal linesS;
    return (renderL lines)

fun render t = renderS t.Head

fun write t =
    oldTail <- get t.Tail;
    newTail <- source End;
    set oldTail (Line newTail);
    set t.Tail newTail

fun main () : transaction page =
    b <- create;

    return <xml><body>
      <button onclick={fn _ => write b}/>
      <dyn signal={render b}/>
    </body></xml>
