datatype lines = End | Line of string * source lines

type t = { Head : source lines, Tail : source (source lines) }

val create =
    head <- source End;
    tail <- source head;
    return {Head = head, Tail = tail}

fun renderL lines =
    case lines of
        End => <xml/>
      | Line (line, linesS) => <xml>{[line]}<br/><dyn signal={renderS linesS}/></xml>

and renderS linesS =
    lines <- signal linesS;
    return (renderL lines)

fun render t = renderS t.Head

fun write t s =
    oldTail <- get t.Tail;
    newTail <- source End;
    set oldTail (Line (s, newTail));
    set t.Tail newTail
