datatype option a = None | Some of a

val none : option int = None
val some_1 : option int = Some 1

val f = fn t ::: Type => fn x : option t =>
        case x of None => None | Some x => Some (Some x)

val none_again = f none
val some_1_again = f some_1

val show = fn t ::: Type => fn x : option t => case x of None => "None" | Some _ => "Some"

val page = fn x => <html><body>
        {cdata (show x)}
</body></html>

val main : unit -> page = fn () => <html><body>
        <li><a link={page none_again}>None</a></li>
        <li><a link={page some_1_again}>Some 1</a></li>
</body></html>
