datatype option a = None | Some of a

val none_Hi : option string = None
val some_Hi = Some "Hi"
val none_some_Hi : option (option string) = None
val some_some_Hi = Some some_Hi

val show = fn x => case x of None => "None" | Some x => x

val show2 = fn x => case x of None => "None'" | Some x => show x

val page = fn x => return <html><body>
        {cdata (show x)}
</body></html>

val page2 = fn x => return <html><body>
        {cdata (show2 x)}
</body></html>

val main : unit -> transaction page = fn () => return <html><body>
        <li><a link={page none_Hi}>None1</a></li>
        <li><a link={page some_Hi}>Some1</a></li>
        <li><a link={page2 none_some_Hi}>None2</a></li>
        <li><a link={page2 some_some_Hi}>Some2</a></li>
</body></html>
